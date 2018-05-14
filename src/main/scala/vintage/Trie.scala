package vintage

import vintage.rlp._
import vintage.rlp.Implicits._

sealed trait TrieNode
case object BlankNode extends TrieNode
case class LeafNode(path: List[Int], value: Array[Byte]) extends TrieNode
case class ExtensionNode(path: List[Int], next: Array[Byte]) extends TrieNode
case class BranchNode(children: Array[Array[Byte]]) extends TrieNode

object Trie {
  val Empty = Array[Byte]()
  val Term = 16

  def keyToPath(key: Seq[Byte]): List[Int] =
    key.map { b =>
      val v = java.lang.Byte.toUnsignedInt(b)
      Seq[Int](v / 16, v % 16)
    }.flatten.toList

  def pathToKey(path: Seq[Int]): Array[Byte] = {
    path.grouped(2).map { l => (16 * l(0) + l(1)).toByte }.toArray
  }

  def encodePath(path: List[Int], leaf: Boolean): Array[Byte] = {
    val term = if (leaf) 1 else 0
    val oddLen = path.size % 2
    val flags = 2 * term + oddLen
    val evenPath = if (oddLen == 1) {
      flags::path
    } else {
      flags::0::path
    }
    pathToKey(evenPath)
  }

  def decodePath(k: Array[Byte]): (List[Int], Boolean) = {
    val f = java.lang.Byte.toUnsignedInt(k(0))
    val flags = f / 16
    val oddLen = flags % 2
    val term = flags / 2
    val tail = k.toList.drop(1).flatMap { b =>
      val v = java.lang.Byte.toUnsignedInt(b)
      Array(v / 16, v % 16)
    }
    val shortPath = if (oddLen == 1) {
      (f % 16)::tail
    } else {
      tail
    }
    (shortPath, term == 1)
  }

  private def makeBranchNode(): BranchNode = BranchNode(Array.fill(17)(Array[Byte]()))
}

class Trie(val db: Db) extends AnyVal {
  import Trie._

  def get(root: String, key: Array[Byte]): Array[Byte] = get(Hex.decode(root), key)
  def update(root: String, key: Array[Byte], value: Array[Byte]): Array[Byte] =
    update(Hex.decode(root), key, value)
  def delete(root: String, key: Array[Byte]): Array[Byte] = delete(Hex.decode(root), key)

  def get(root: Array[Byte], key: Array[Byte]): Array[Byte] =
    internalGet(root, keyToPath(key))

  def update(root: Array[Byte], key: Array[Byte], value: Array[Byte]): Array[Byte] =
    putNode(internalUpdate(root, keyToPath(key), value), true)

  def delete(root: Array[Byte], key: Array[Byte]): Array[Byte] =
    putNode(internalDelete(root, keyToPath(key)), true)

  def dump(root: String): Map[String, String] =
    internalDump(Hex.decode(root))
      .map { case (k, v) => Hex.encode(pathToKey(k)) -> Hex.encode(v) }.toMap

  private def internalGet(node: Array[Byte], path: List[Int]): Array[Byte] = {
    getNode(node) match{
      case BlankNode => Empty
      case LeafNode(npath, nvalue) => {
        if (npath == path) {
          nvalue
        } else {
          Empty
        }
      }
      case ExtensionNode(npath, nvalue) => {
        if (path.startsWith(npath)) {
          internalGet(nvalue, path.drop(npath.size))
        } else {
          Empty
        }
      }
      case BranchNode(children) => {
        if (path.isEmpty) {
          children(Term)
        } else {
          internalGet(children(path(0)), path.tail)
        }
      }
    }
  }

  private def internalUpdate(node: Array[Byte], path: List[Int], value: Array[Byte])
      : TrieNode = {
    // TODO: prune node
    getNode(node) match {
      case BlankNode => LeafNode(path, value)
      case BranchNode(children) => {
        if (path.isEmpty) {
          children(Term) = value
        } else {
          children(path.head) = putNode(internalUpdate(children(path.head), path.tail, value))
        }
        BranchNode(children)
      }
      case LeafNode(p, v) => updateExtNode(p, v, true, path, value)
      case ExtensionNode(p, v) => updateExtNode(p, v, false, path, value)
    }
  }

  private def updateExtNode(
      extPath: List[Int], extValue: Array[Byte], leaf: Boolean,
      updatePath: List[Int], value: Array[Byte])
      : TrieNode = {
    val maxPrefix = Math.min(extPath.size, updatePath.size)
    val prefixLen = 0.until(maxPrefix).find(i => extPath(i) != updatePath(i)).getOrElse(maxPrefix)
    val prefix = extPath.take(prefixLen)
    val extRem = extPath.drop(prefixLen)
    val updateRem = updatePath.drop(prefixLen)
    val br = if (extRem.isEmpty && updateRem.isEmpty) {
      if (leaf) {
        LeafNode(updateRem, value)
      } else {
        internalUpdate(extValue, updateRem, value)
      }
    } else if (extRem.isEmpty) {
      if (leaf) {
        val pos = updateRem.head
        val newLeaf = LeafNode(updateRem.tail, value)
        val branch = makeBranchNode()
        branch.children(Term) = extValue
        branch.children(pos) = putNode(newLeaf)
        branch
      } else {
        internalUpdate(extValue, updateRem, value)
      }
    } else {
      val branch = makeBranchNode()
      if (extRem.size == 1 && !leaf) {
        branch.children(extRem.head) = extValue
      } else {
        val n = if (leaf) LeafNode(extRem.tail, extValue) else ExtensionNode(extRem.tail, extValue)
        branch.children(extRem.head) = putNode(n)
      }
      if (updateRem.isEmpty) {
        branch.children(Term) = value
      } else {
        branch.children(updateRem.head) = putNode(LeafNode(updateRem.tail, value))
      }
      branch
    }
    if (prefix.isEmpty) {
      br
    } else {
      ExtensionNode(prefix, putNode(br))
    }
  }

  private def internalDelete(nodeKey: Array[Byte], path: List[Int]): TrieNode = {
    // TODO: prune node
    val node = getNode(nodeKey)
    node match {
      case BlankNode => BlankNode
      case BranchNode(children) => deleteBranchNode(children, path)
      case LeafNode(p, v) => deleteExtNode(node, p, v, true, path)
      case ExtensionNode(p, v) => deleteExtNode(node, p, v, false, path)
    }
  }

  private def deleteBranchNode(children: Array[Array[Byte]], path: List[Int]): TrieNode = {
    if (path.isEmpty) {
      children(Term) = Empty
      normalizeBranchNode(children)
    } else {
      val subNode = putNode(internalDelete(children(path.head), path.tail))
      children(path.head) = subNode
      if (subNode == Empty) {
        normalizeBranchNode(children)
      } else {
        BranchNode(children)
      }
    }
  }

  private def deleteExtNode(
      node: TrieNode, extPath: List[Int], extValue: Array[Byte], leaf: Boolean,
      delPath: List[Int]): TrieNode = {
    if (!delPath.startsWith(extPath)) {
      node
    } else if (leaf) {
      if (extPath == delPath) {
        BlankNode
      } else {
        node
      }
    } else {
      val subNode = internalDelete(extValue, delPath.drop(extPath.size))
      subNode match {
        case BlankNode => BlankNode
        case LeafNode(p, v) => LeafNode(extPath ++ p, v)
        case ExtensionNode(p, v) => ExtensionNode(extPath ++ p, v)
        case BranchNode(children) => ExtensionNode(extPath, putNode(subNode))
      }
    }
  }

  private def normalizeBranchNode(children: Array[Array[Byte]]): TrieNode = {
    // A branch node which is left with only a single non-blank item should be
    // turned into either a leaf or extension node.
    val nonEmpty = children.zipWithIndex.filter { case (v, i) => !v.isEmpty }
    if (nonEmpty.size > 1) {
      BranchNode(children)
    } else {
      val (v, i) = nonEmpty(0)
      if (i == Term) {
        LeafNode(List(), v)
      } else {
        val child = getNode(v)
        // TODO: prune child for leaf/ext
        child match {
          case LeafNode(p, v) => LeafNode(i::p, v)
          case ExtensionNode(p, v) => ExtensionNode(i::p, v)
          case BranchNode(_) => ExtensionNode(List(i), v)
          case _ => ???
        }
      }
    }
  }

  private def putNode(node: TrieNode, forceHash: Boolean = false): Array[Byte] = {
    if (node == BlankNode) {
      Empty
    } else {
      val rlp = node match {
        case BlankNode => ???
        case LeafNode(path, value) =>
          RLPList(Vector[RLPObject](encodePath(path, true), value))
        case ExtensionNode(path, value) =>
          RLPList(Vector[RLPObject](encodePath(path, false), value))
        case BranchNode(children) =>
          RLPList(children.map(RLPBytes(_)).toVector)
      }
      val value = RLPCodec.encode(rlp)
      if (!forceHash && value.size < 32) {
        value
      } else {
        Hex.decode(db.put(value))
      }
    }
  }

  private def getNode(k: Array[Byte]): TrieNode = {
    if (k.size == 0) {
      BlankNode
    } else {
      val value = if (k.size < 32) k else db.get(k)
      val list: Vector[RLPObject] = RLPCodec.decode(value)
      if (list.size == 17) {
        BranchNode(list.map(a => a: Array[Byte]).toArray)
      } else {
        val (path, leaf) = decodePath(list(0): Array[Byte])
        val v = list(1): Array[Byte]
        if (leaf) {
          LeafNode(path, v)
        } else {
          ExtensionNode(path, v)
        }
      }
    }
  }

  private def internalDump(node: Array[Byte], prefix: Vector[Int] = Vector())
      : Seq[(Vector[Int], Array[Byte])] = {
    getNode(node) match {
      case BlankNode => Seq()
      case BranchNode(children) => {
        val nodes = (0 until Term).flatMap(i => {
          if (children(i).isEmpty) Seq() else internalDump(children(i), prefix :+ i)
        })
        if (children(Term).isEmpty) nodes else nodes :+ (prefix -> children(Term))
      }
      case LeafNode(path, value) => Seq(prefix ++ path -> value)
      case ExtensionNode(path, next) => internalDump(next, prefix ++ path)
    }
  }
}
