package io.aecor.liberator

import io.aecor.liberator.Algebra.AlgebraSyntax
import io.aecor.liberator.FunctorK.FunctorKSyntax
import io.aecor.liberator.data.ProductKK.ProductKKSyntax

object syntax extends TermSyntax with AlgebraSyntax with ProductKKSyntax with FunctorKSyntax
