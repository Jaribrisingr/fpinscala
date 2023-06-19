package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {

  //exercise 3.35
  def size[A](tree: Tree[A]): Int = {
    tree match {
      case Leaf(_) => 1
      case Branch(l, r) => 1 + size(l) + size(r)
    }
  }

  //exercise 3.26
  def maximum(tree: Tree[Int]): Int = {
    tree match {
      case Leaf(x) => x
      case Branch(l, r) => maximum(l) max maximum(r)
    }
  }

  // exercise 3.27
  def depth[A](tree: Tree[A]): Int = {
    tree match {
      case Leaf(x) => 0
      case Branch(l, r) => 1 + (depth(l) max depth(r))
    }
  }

  // exercise 3.28
  def map[A,B](t: Tree[A])(f: A => B): Tree[B] = {
    t match {
      case Leaf(x) => Leaf(f(x))
      case Branch(l,r) => Branch(map(l)(f), map(r)(f))
    }
  }

  def fold[A, B](t: Tree[A])(f: A => B)(g: (B, B) => B): B = t match {
    case Leaf(a) => f(a)
    case Branch(l, r) => g(fold(l)(f)(g), fold(r)(f)(g))
  }

  // exercise 3.29
  def foldSize[A](tree: Tree[A]): Int = {
    fold(tree)(a => 1)(1 + _ + _)
  }
  def foldMax(tree: Tree[Int]): Int = {
    fold(tree)(a => a)(_ max _)
  }
  def foldDepth[A](t: Tree[A]): Int =
    fold(t)(a => 0)((d1, d2) => 1 + (d1 max d2))

  def foldMap[A, B](t: Tree[A])(f: A => B): Tree[B] =
    fold(t)(a => Leaf(f(a)): Tree[B])(Branch(_, _))
}