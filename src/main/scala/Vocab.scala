package org.bottomup.arithmetic

import Arithmetic.Expr

import scala.collection.mutable

case class Vocab(leafNodes: List[Expr], operations: mutable.Map[String, Double])
