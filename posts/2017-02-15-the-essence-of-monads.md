---
title: The essence of monads
author: Omar Diego Vera Ustariz
description: Presents and overview of what monads are and describes the main idea of what are monads for
---

Monads are really a great concept from functional programming that is even used in other programming languages with another names. For example some people says that Promises in Javascript are monads. In F# there is Rail Oriented Programming for error propagation, which turn out to be monads as well.
There are several tutorials, blogs and papers around there which try to explain what monads are. I learned a lot reading some of them, however, I still had the question: what is the essence of a monad?.

Everytime I was reading something about monads I got the feeling that they were just describing a characteristic of a monad or they were describing the implementation details of a monad.

In this blog I would like to focus on the overview of what monads are without discussing its implementation details.

#### Purity and effects
Monads were introduced in Haskell because they had a problem of expressing functions with effects as pure functions.
For example consider the following code written in a non-functional style:

``` Java
public int divide(int a, int b)
{
  return a%b;
}
```

From a functional programming point of view this code is not pure because it says it can divide two integers. However, we know that it would fail when b is zero.
In pure functional languages they would like to define the function in the following way:
``` Haskell
divide :: Int -> Int -> Int
```

However, from the view of a pure functional language that function is not pure, because it is not defined when the second argument is zero.

A pure function is like in mathematics, it is always defined for all the inputs. Pure functions are what we learn at school in mathematics. We get always a result, they do not know about throwing exceptions. Purity is a nice feature because is makes programs more predictable and reading and understanding the code is easily than in non-pure languages.

A pure function had the following form:
``` Haskell
  f :: a -> b
```

It means for any *a* (it could have more inputs but for simplicity we take just one) give us as a result b.

Pure languages introduced a trick to convert a non-pure function into a pure function. Saying that a non-pure function is pure if they have the following format:
``` Haskell
  f :: a -> M b
```

M is the context that describes the effect that could make impure our function and at the same time guaranties that the function will always return a defined value. Some people says that the M is like a box that **may** contain a result of a computation when it succeeds or another value when it fails. M is the guarantee that the function will give us always a result.

Using this trick we can rewrite our function in the following way:
``` Haskell
  divide :: a -> b -> Maybe c
```

Notice that the most important is that the function returns something in the format *M b*, the function can have more arguments but the most important is the it returns something of the form *M b*.

Maybe is defined in the following way:
``` Haskell
data Maybe = Just a | Nothing
```

Maybe is an abstract data type that has two constructors Just, which needs an *a* as input or *Nothing*. *Just* is like a box containing a value a and Nothing would be and empty box.

So, what we described was the motivation to use Maybe as a result instead of an Integer. In that way the function divide is now pure.

Maybe represents just one type of effect that we can have, the effect here is the absence of a result. However, when we are programming we have more functions with effects like:

* Functions with IO functionality. A function that reads a file could return different values, e.g when the content of a file has changed.

* Mutability. Changing the values of variables cause functions to not be pure anymore. E.g when using global variables. Mutability represents the change of state which is not desireable in functional programming because it is a cause that can make functions impure.

* Indeterminism. A function of the type a -> [b] can be seen as causing an effect as well, because for a type *a* produces a list of outputs *[b]*. Therefore, it is nondeterministic because that function delivers a set of possible results.

Those are just samples of effects, however, in programming there is a lot more effects.

#### All monads have have something in common
From the previously analized functions we can write them in the following way:

``` Haskell
-- Effect: Absence of result
f1 :: a -> Maybe b
-- In general it has the form
f1:: a -> M b

-- Effect: Functions with IO
f2 :: a -> IO b
-- In general it has the form
f2 :: a -> M b

-- Effect: Mutability
f3 :: (a, s) -> (b, s)
-- When applying currying can be seen as
f3 :: a -> s -> (b, s)
-- Or it can be seen as well as
f3 :: a -> (s -> (b, s))
-- It can be represented as well in the following form
f3 :: a -> M b

-- Effect Indeterminism
f4 :: a -> [b]
-- In general it has the form
f4 :: a -> M b
```

From all the monads, the one that represents the mutability is known as State monad as well. Maybe for this one is not so easy to see that it has the same format as the other monads. However, with the trick of currying it is possible to see that is similar to the other monads.

#### The essence of monads

Now that we have seen that the monads have really the form:
``` Haskell
  f :: a -> M b
```
Where M represents some embellishment that helped us to convert an impure function into a pure one. Pure function means that for every a the function will give us always a result. The M is the monad that abstract one kind of effect. This effect could be the possible absence of a result, IO, mutability, indeterminism or any other.

Now the question that we have is how to combine that functions? And another more interesting question would be why do we need to combine them?

The answer to this question is about what software engineering is or how we as human beings solve problems. What we do to deal with complexity is to split a problem into several small pieces and later we need to combine the small solutions to get the complete solution.

Every monadic function that has the form *a -> M b* solves a small problem. However, to get the final solution of a problem we will need to combine several monadic functions that has the form *a -> M b*.

And finally we can say that the essence of monads is the *composition* of function of the form *a -> M b*. We can see that in the definition of a monad.

A Monad can be defined as a type class that has two function, the fish operator (>=>) and return.

``` Haskell
  (>=>) :: (a -> M b) -> (b -> M c) -> (a -> M c)
  return :: a -> M a
```

In the fish operator we can see that it is really composing two functions *a -> M b* and the function *b -> M c*. *return* converts a non monadic value into a monadic one. Classically a monad is defined using a bind operator (>>=) and return. However, with the previous definition it is easy to see the essence of a monad.

#### Conclusion
In this blog we have seen that the essence of monads is the composition of functions that have some effects. The effects could be different. We just mentioned some of them in this blog, because effects can be from different nature.

This blog contains some theory but it was necessary to help us to understand the nature of monads. The theory help us to make practical things in a better way.

I did not mention it before but the function of the form *a -> M b* are known as Kleisli arrows. Using a definition of monads that combine Kleisli arrows helped us as well to see the essence of monads.

#### References
[Monads and functional programming.](http://homepages.inf.ed.ac.uk/wadler/papers/marktoberdorf/baastad.pdf) Philip Wadler, University of Glasgow.

[Monads: Programmer’s Definition.](https://bartoszmilewski.com/2016/11/21/monads-programmers-definition/) Bartosz Milewski. Programming Cafe.
