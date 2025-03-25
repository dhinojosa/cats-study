# Cats-Study

## About

Project housing various samples for typelevel cats. Visit https://typelevel.org/cats for more information on the project

## License

This project is under the MIT License.  Cats seems to also be under the MIT 
License with attribution to the license made for Scalaz

## Running

To run, select a test to run and in sbt run the commands:

### From the Terminal

`sbt reload update`

`sbt testOnly com.xyzcorp.<TEST-NAME>`

or to select which test:

`sbt testOnly com.xyzcorp.<TEST-NAME> -- -z '<SUBSTRING>'`

for example,

`sbt testOnly com.xyzcorp.typeclasses.FunctorSpec -- -z 'Case 1:'`

### Within SBT

`;reload;update`

`testOnly com.xyzcorp.<TEST-NAME>`

or to select which test with a substring:

`testOnly com.xyzcorp.<TEST-NAME> -- -z '<SUBSTRING>'`

for example,

`testOnly com.xyzcorp.typeclasses.FunctorSpec -- -z 'Case 1:'`

## Rules 

### How each `Spec` should look

1. Each `Spec` should only encompass what it owns. In other words, `Apply` extends `Functor`, therefore `Apply` should only show what is new or different in `Apply`, if you want to view information on `Functor` then go to `FunctorSpec`
2. It should start with a methods it contains with a sample how it should run.  
3. Then it should show any extensions that may be used with Scala standard library.
4. Any laws that are applicable.

## TypeClass Shapes

The following are the shapes typically found in Typelevel Cats and their behaviors

| TypeClass        | Shape                      | Description                                                                  |
|------------------|----------------------------|------------------------------------------------------------------------------|
| `Functor`        | `F[A]`                     | A type constructor that can be mapped over.                                  |
| `Applicative`    | `F[A]`                     | Extends `Functor` with the ability to lift pure values and combine contexts. |
| `Apply`          | `F[A]`                     | Extends `Functor`, combining contexts without introducing new ones.          |
| `Monad`          | `F[A]`                     | Extends `Applicative`, supporting flat-mapping and chaining computations.    |
| `Kleisli`        | `A => F[B]`                | Represents a function `A => F[B]`, often used for combining computations.    |
| `Contravariant`  | `F[A]`                     | Similar to `Functor`, but maps functions in the opposite direction.          |
| `Invariant`      | `F[A]`                     | Supports bidirectional mapping, useful for codecs, etc.                      |
| `SemigroupK`     | `F[A]`                     | Combines two `F[A]` values, useful for type constructors.                    |
| `MonoidK`        | `F[A]`                     | Extends `SemigroupK`, providing an identity element.                         |
| `Foldable`       | `F[A]`                     | Supports folding (reducing) values within a context.                         |
| `Traverse`       | `F[A]`                     | Combines `Functor` and `Foldable`. Can traverse and map sequentially.        |
| `Parallel`       | `(F[_], G[_])`             | Provides parallel composition of type constructors.                          |
| `Eval`           | `A`                        | A value that supports lazy and memoized computation.                         |
| `Reader`         | `A => B`                   | Encapsulates a context-dependent computation.                                |
| `Writer`         | `(W, A)`                   | Captures a log (`W`) alongside a value (`A`).                                |
| `State`          | `S => (S, A)`              | Encapsulates state transitions.                                              |
| `StateT`         | `S => F[(S, A)]`           | A monadic transformer for state transitions.                                 |
| `OptionT`        | `F[Option[A]]`             | A monad transformer for `Option`.                                            |
| `EitherT`        | `F[Either[A, B]]`          | A monad transformer for `Either`.                                            |
| `Id`             | `A`                        | The identity type constructor.                                               |
| `Validated`      | `Either[List[E], A]`       | Represents validation with either errors (`E`) or a valid value.             |
| `Ior`            | `Either[A, B]` or `(A, B)` | Represents inclusive-or (both, left, or right).                              |
| `NonEmptyList`   | `List[A]`                  | A `List` guaranteed to have at least one element.                            |
| `NonEmptyChain`  | `Chain[A]`                 | A `Chain` guaranteed to have at least one element.                           |
| `NonEmptyVector` | `Vector[A]`                | A `Vector` guaranteed to have at least one element.                          |
| `Chain`          | `Seq[A]`                   | An immutable and efficient sequence implementation.                          |
| `Free`           | `F[A]`                     | Represents a pure computation over a functor.                                |
| `Cofree`         | `F[A]`                     | Cofree comonads, the dual of `Free`.                                         |
| `Yoneda`         | `F[A]`                     | Functor optimization for mapping.                                            |
| `Coyoneda`       | `F[A]`                     | Functor optimization by precomputing maps.                                   |
| `Const`          | `C[A, B]` or `C`           | Captures a constant value, often used in `Foldable` or `Traverse`.           |

| `CoKleisli`      | `F[A] => B`                | Represents a function from a context `F[A]` to a value `B`. |