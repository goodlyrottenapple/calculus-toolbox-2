<img src="./gui/public/icon.svg" height="400">

# calculus-toolbox-2
This is the new, much improved version of the original [calculus-toolbox](https://github.com/goodlyrottenapple/calculus-toolbox). The new version isn't quite feature ready, but contains most of the functionality of the original UI tool, whilst greatly simplifying the compilation of new calculi. Unlike the old toolbox, the new one also offers support for (finitary?) multitype display calculi, as discussed in the following [paper](http://www.cs.le.ac.uk/people/ak155/Papers/multi-type-deak.pdf).   
The new tool can load and modify display calculi at runtime, with no need for re-compilation.

## Download and Installation 
To get started, please download the [alpha version](https://github.com/goodlyrottenapple/calculus-toolbox-2/releases) for your operating system. For Windows users, when launching the toolbox for the first time, please enable the two firewall windows that pop up.

## A brief introduction

Click on the image below for a brief video introducing the core features of the Calculus toolbox UI:
[![Calculus toolbox introduction video](https://img.youtube.com/vi/I-lot7xdkVM/0.jpg)](https://www.youtube.com/watch?v=I-lot7xdkVM)

(https://www.youtube.com/watch?v=I-lot7xdkVM)

## Calculus definition
Calculi can be specified within the tool in a DSL, separated into two definitions, first defining the types and connectives of the calculus and then defining the rules.

### Definitions
A minimal calculus needs to specify at least one type, usually the type of atomic propositions. This is just an abstract type and for single type calculi, it is enough to write

`type atprop`

As display calculi have two levels of connectives, namely formula level connectives and structural connectives, the type signature of the connective will determine the level it belongs to:

`bot : formula ("0", NonAssoc, 10, "\bot")`

The type signature above defines a nullary connective, or a constant, at the formula level. In order for the type to be well-formed, further information is required:

* `"0"` is the ASCII parser sugar. It is possible to define mixfix syntax, similar to Agda or Isabelle, by providing a string with 0 or more holes, denoted by an underscore.
* `NonAssoc` defines the associativity of the operator (for binary connectives, the other two options are `LeftAssoc` and `RightAssoc`).
* `10` defines how tightly the operator binds.
* `"\bot"` defines the LaTeX typesetting macro (if the connective is non-nullary, the arguments are referenced in the same way as LaTeX macros, namely `#1`, `#2`, etc.). For the supported subset of LaTeX functions, see [here](https://khan.github.io/KaTeX/function-support.html).

We can define a conjunction in the following way: 

`and : formula -> formula -> formula ("_/\_", LeftAssoc, 2, "#1 \land #2")`

In order to define a connective at the structural level, say the comma, we give it the following type signature:

`comma : structure -> structure -> structure ("_,_", LeftAssoc, 2, "#1\,,\,#2")`

Note that the connectives must be homogeneous, namely, we cannot define the following one:

`bad : formula -> structure -> formula ("_??_", LeftAssoc, 2, "#1\ ?? #2")`

since it's type contains both `formula`s and `structure`s.

However, if we define multiple types, like in the case of the multitype display calculus [D.EAK](http://www.cs.le.ac.uk/people/ak155/Papers/multi-type-deak.pdf) where we have the type of atprops and agents, we can define the box connective in the following way:

```
default type atprop
type agent
box : formula{agent} -> formula{atprop} -> formula ("[_]_" , NonAssoc, 4, "\Box_{#1} #2") 
```

Notice that we declared `atprop` to be the default type (if there is more than one type in the definition, one has to be declared with the keyword `default`) and can thus omit `{atprop}` in `...-> formula ("[_...`, which is equivalent to `...-> formula{atprop} ("[_...`.

### Rules
The rules can be defined in the style of natural deduction, with the premises and conclusion separated with one or more dashes, followed by the name of the rule. For example, we can define the atom rule as:

```
------------ id
at_A |- at_A
```

Prefixing the variables `A` with `at_` lets the toolbox know that `A` should be an atom variable. Similarly, we can define the `andL` rule, where `A` and `B` are both formula variables and `X` is a structure variable:

```
f_A ,  f_B |- s_X
----------------- andL
f_A /\ f_B |- s_X
```

However, we can drop this notation in the case above, as the tool can automatically figure out the most general level for each variable:

```
A ,  B |- X
----------- andL
A /\ B |- X
```

When processing the rules, the tool will try to assign each variable the most general/highest level, namely that of a structure variable. However, if we define `/\` as a formula connective, it will take this into account and lower `A` and `B` to formula variables.   
Writing `s_A` instead of `A` would therefore result in an error. This is also true if we try to use the same variable at two different types:

```
 A /\ B |- X
------------ bad
[ A ] B |- X
```

This rule would be rejected, since the tool would not be able to unify `A` in the premise, which must have the type (of at least) `formula{atprop}`, with the type of `A` in the conclusion, which has to be a `formula{agent}` according to the definitions conjunction and the box, respectively.

A rule with multiple premises can be written like this:

```
X |- A    Y |- B
---------------- andR
X , Y |- A /\ B
```

## Notes

### Parser whitespace
Since the parser admits mixfix notation, whitespace is important. Namely, the parser will not usually be able to parse `a/\b` or `[ag1]b` correctly, as the tokenizer will treat `a/\b` as a full variable name (this is similar behaviour to Agda). Instead, one has to write `a /\ b` and `[ ag1 ] b`, for the intended parse.

### Abbreviations
You can now add formula abbreviations inside the tool, to make working with sequents containing long formulas simpler. Abbreviations hide the defined formula behind a user given label. For example, the following sequent, containing abbreviation (highlighed in green)

![abbrev](https://user-images.githubusercontent.com/10553895/35685320-47bb2dc8-0761-11e8-94ad-6602069f5fa9.jpeg)

was partially expanded to:

![abbrev_part_expanded](https://user-images.githubusercontent.com/10553895/35685315-452543be-0761-11e8-8af5-5a8bd91dc08b.jpeg)

Leading to a rather long formula, which is not very readable.

To add an abbreviation, open the sidebar (the cog wheel icon in top right corner) and click on ‘Add a formula abbreviation’. Since the tool supports multiple types for the sequents, the first dropdown will ask you to select the type of the formula you want to define.

The syntax for parsing an abbreviation in the bottom bar (or when adding another abbreviation, as abbreviations can be nested) is the following:

If you declare an abbreviation of type `Fm`, called `D_1`, where, say, the formula is <img src="https://user-images.githubusercontent.com/10553895/35685549-f09692e8-0761-11e8-86ba-d60fe62dddf1.jpeg" height="25"> you can tell the parser to parse `D_1` as an abbreviation by putting it in double curly braces:

```
{{D_1}} |- C_1 -> 0
```

This will be different from:

```
D_1 |- C_1 -> 0
```

As the former will substitute `{{D_1}}` with an abbreviation `D_1` which hides the formula <img src="https://user-images.githubusercontent.com/10553895/35685549-f09692e8-0761-11e8-86ba-d60fe62dddf1.jpeg" height="25">.

### Sample calculus description file
If you want to test the functionality of the tool without having to define a display calculus from scratch, try downloading the display version of the [classical LK sequent calculus](https://github.com/goodlyrottenapple/calculus-toolbox-2/releases/download/v0.1.0-alpha/DispLK.zip). Extract the archive and place `DispLK.calc` and `DispLK.rules` into the working directory you chose in the initial setup window.

### Windows Firewall
When opening the toolbox for the first time on Windows, you will see two firewall dialogs like this:

![windows firewall](https://user-images.githubusercontent.com/10553895/34380814-7a5a8748-eb04-11e7-9589-667268a3a349.PNG)

Please allow access to both. These pop up, because the core toolbox tool is running an http server, which talks to the GUI frontend.

### Logo
The calculus toolbox logo is licenced under a CC license, and was created by [Gilda Martini](https://thenounproject.com/gildamartini/collection/fauna-precolombina/?i=138974)
