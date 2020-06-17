{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
module Language.OCaml.Tags
( ToTags(..)
) where

import           AST.Element
import           AST.Token
import           AST.Traversable1
import           Control.Effect.Reader
import           Control.Effect.Writer
import qualified Language.OCaml.AST as OCaml
import           Source.Loc
import           Source.Source as Source
import           Tags.Tag()
import qualified Tags.Tagging.Precise as Tags

class ToTags t where
  tags
    :: ( Has (Reader Source) sig m
       , Has (Writer Tags.Tags) sig m
       )
    => t Loc
    -> m ()
  default tags
    :: ( Has (Reader Source) sig m
       , Has (Writer Tags.Tags) sig m
       , Traversable1 ToTags t
       )
    => t Loc
    -> m ()
  tags = gtags

instance (ToTags l, ToTags r) => ToTags (l :+: r) where
  tags (L1 l) = tags l
  tags (R1 r) = tags r

instance ToTags (Token sym n) where tags _ = pure ()

gtags
  :: ( Has (Reader Source) sig m
     , Has (Writer Tags.Tags) sig m
     , Traversable1 ToTags t
     )
  => t Loc
  -> m ()
gtags = traverse1_ @ToTags (const (pure ())) tags

instance ToTags OCaml.AbstractType
instance ToTags OCaml.Arguments
instance ToTags OCaml.ArrayExpression
instance ToTags OCaml.ArrayType
instance ToTags OCaml.AssignmentExpression
instance ToTags OCaml.AssociatedType
instance ToTags OCaml.AsyncBlock
instance ToTags OCaml.AttributeItem
instance ToTags OCaml.AwaitExpression
instance ToTags OCaml.BaseFieldInitializer
instance ToTags OCaml.BinaryExpression
instance ToTags OCaml.Block
instance ToTags OCaml.BlockComment
instance ToTags OCaml.BooleanLiteral
instance ToTags OCaml.BoundedType
instance ToTags OCaml.BracketedType
instance ToTags OCaml.BreakExpression
instance ToTags OCaml.CallExpression
instance ToTags OCaml.CapturedPattern
instance ToTags OCaml.CharLiteral
instance ToTags OCaml.ClosureExpression
instance ToTags OCaml.ClosureParameters
instance ToTags OCaml.CompoundAssignmentExpr
instance ToTags OCaml.ConstItem
instance ToTags OCaml.ConstParameter
instance ToTags OCaml.ConstrainedTypeParameter
instance ToTags OCaml.ContinueExpression
instance ToTags OCaml.Crate
instance ToTags OCaml.DeclarationList
instance ToTags OCaml.DeclarationStatement
instance ToTags OCaml.DynamicType
instance ToTags OCaml.EmptyStatement
instance ToTags OCaml.EmptyType
instance ToTags OCaml.EnumItem
instance ToTags OCaml.EnumVariant
instance ToTags OCaml.EnumVariantList
instance ToTags OCaml.EscapeSequence
instance ToTags OCaml.Expression
instance ToTags OCaml.ExternCrateDeclaration
instance ToTags OCaml.ExternModifier
instance ToTags OCaml.FieldDeclaration
instance ToTags OCaml.FieldDeclarationList
instance ToTags OCaml.FieldExpression
instance ToTags OCaml.FieldIdentifier
instance ToTags OCaml.FieldInitializer
instance ToTags OCaml.FieldInitializerList
instance ToTags OCaml.FieldPattern
instance ToTags OCaml.FloatLiteral
instance ToTags OCaml.ForExpression
instance ToTags OCaml.ForLifetimes
instance ToTags OCaml.ForeignModItem
instance ToTags OCaml.FragmentSpecifier
instance ToTags OCaml.FunctionItem
instance ToTags OCaml.FunctionModifiers
instance ToTags OCaml.FunctionSignatureItem
instance ToTags OCaml.FunctionType
instance ToTags OCaml.GenericFunction
instance ToTags OCaml.GenericType
instance ToTags OCaml.GenericTypeWithTurbofish
instance ToTags OCaml.HigherRankedTraitBound
instance ToTags OCaml.Identifier
instance ToTags OCaml.IfExpression
instance ToTags OCaml.IfLetExpression
instance ToTags OCaml.ImplItem
instance ToTags OCaml.IndexExpression
instance ToTags OCaml.InnerAttributeItem
instance ToTags OCaml.IntegerLiteral
instance ToTags OCaml.LetDeclaration
instance ToTags OCaml.Lifetime
instance ToTags OCaml.LineComment
instance ToTags OCaml.Literal
instance ToTags OCaml.LiteralPattern
instance ToTags OCaml.LoopExpression
instance ToTags OCaml.LoopLabel
instance ToTags OCaml.MacroDefinition
instance ToTags OCaml.MacroInvocation
instance ToTags OCaml.MacroRule
instance ToTags OCaml.MatchArm
instance ToTags OCaml.MatchBlock
instance ToTags OCaml.MatchExpression
instance ToTags OCaml.MatchPattern
instance ToTags OCaml.MetaArguments
instance ToTags OCaml.MetaItem
instance ToTags OCaml.Metavariable
instance ToTags OCaml.ModItem
instance ToTags OCaml.MutPattern
instance ToTags OCaml.MutableSpecifier
instance ToTags OCaml.NegativeLiteral
instance ToTags OCaml.OptionalTypeParameter
instance ToTags OCaml.OrderedFieldDeclarationList
instance ToTags OCaml.Parameter
instance ToTags OCaml.Parameters
instance ToTags OCaml.ParenthesizedExpression
instance ToTags OCaml.Pattern
instance ToTags OCaml.PointerType
instance ToTags OCaml.PrimitiveType
instance ToTags OCaml.QualifiedType
instance ToTags OCaml.RangeExpression
instance ToTags OCaml.RangePattern
instance ToTags OCaml.RawStringLiteral
instance ToTags OCaml.RefPattern
instance ToTags OCaml.ReferenceExpression
instance ToTags OCaml.ReferencePattern
instance ToTags OCaml.ReferenceType
instance ToTags OCaml.RemainingFieldPattern
instance ToTags OCaml.RemovedTraitBound
instance ToTags OCaml.ReturnExpression
instance ToTags OCaml.ScopedIdentifier
instance ToTags OCaml.ScopedTypeIdentifier
instance ToTags OCaml.ScopedUseList
instance ToTags OCaml.Self
instance ToTags OCaml.SelfParameter
instance ToTags OCaml.ShorthandFieldIdentifier
instance ToTags OCaml.ShorthandFieldInitializer
instance ToTags OCaml.SlicePattern
instance ToTags OCaml.SourceFile
instance ToTags OCaml.StaticItem
instance ToTags OCaml.StringLiteral
instance ToTags OCaml.StructExpression
instance ToTags OCaml.StructItem
instance ToTags OCaml.StructPattern
instance ToTags OCaml.Super
instance ToTags OCaml.TokenBindingPattern
instance ToTags OCaml.TokenRepetition
instance ToTags OCaml.TokenRepetitionPattern
instance ToTags OCaml.TokenTree
instance ToTags OCaml.TokenTreePattern
instance ToTags OCaml.TraitBounds
instance ToTags OCaml.TraitItem
instance ToTags OCaml.TryExpression
instance ToTags OCaml.TupleExpression
instance ToTags OCaml.TuplePattern
instance ToTags OCaml.TupleStructPattern
instance ToTags OCaml.TupleType
instance ToTags OCaml.Type
instance ToTags OCaml.TypeArguments
instance ToTags OCaml.TypeBinding
instance ToTags OCaml.TypeCastExpression
instance ToTags OCaml.TypeIdentifier
instance ToTags OCaml.TypeItem
instance ToTags OCaml.TypeParameters
instance ToTags OCaml.UnaryExpression
instance ToTags OCaml.UnionItem
instance ToTags OCaml.UnitExpression
instance ToTags OCaml.UnitType
instance ToTags OCaml.UnsafeBlock
instance ToTags OCaml.UseAsClause
instance ToTags OCaml.UseDeclaration
instance ToTags OCaml.UseList
instance ToTags OCaml.UseWildcard
instance ToTags OCaml.VariadicParameter
instance ToTags OCaml.VisibilityModifier
instance ToTags OCaml.WhereClause
instance ToTags OCaml.WherePredicate
instance ToTags OCaml.WhileExpression
instance ToTags OCaml.WhileLetExpression
