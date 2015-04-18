package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {

  def size[A](tree: Tree[A]): Int =
    tree match {
      case Leaf(_) => 1
      case Branch(left, right) => 1 + size(left) + size(right)
    }

  def maximum(tree: Tree[Int]): Int =
    tree match {
      case Leaf(value) => value
      case Branch(left, right) => maximum(left) max maximum(right)
    }

  def depth[A](tree: Tree[A]): Int =
    tree match {
      case Leaf(_) => 1
      case Branch(left, right) => depth(left).max(depth(right)) + 1
    }

  def map[A, B](tree: Tree[A])(f: A => B): Tree[B] =
    tree match {
      case Leaf(value) => Leaf(f(value))
      case Branch(left, right) => Branch(map(left)(f), map(right)(f))
    }

  def fold[A, B](tree: Tree[A])(l: A => B)(b: (B, B) => B): B =
    tree match {
      case Leaf(value) => l(value)
      case Branch(left, right) => b(fold(left)(l)(b), fold(right)(l)(b))
    }

  def size1[A](tree: Tree[A]): Int =
    fold(tree) { _ => 1} { _ + _  + 1 }

  def maximum1(tree: Tree[Int]): Int =
    // fold(tree)(_) { (x, y) => x max y } this doesn't work
    // fold(tree)(_ => _) { (x, y) => x max y }
    fold(tree)(l => l) { _ max _ }

  def depth1[A](tree: Tree[A]): Int =
    fold(tree)(_ => 1) { (x, y) => x.max(y) + 1 }

  def map1[A, B](tree: Tree[A])(f: A => B): Tree[B] =
    fold(tree) { l => Leaf(f(l)): Tree[B] } { (x, y) => Branch(x, y) }
}