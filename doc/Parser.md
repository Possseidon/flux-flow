# Parser

This document outlines the internal implementation of the parsing process. The main reason for this document is the fact, that at some point I was no longer able keep the entire process in my head at once.

## Grammar

A grammar is made up of a set of named recursive rules. One of them (the first one) is considered the root rule.

### `RecursiveRule`

Recursive rules allow for rules to be made up of other rules. The contained rules are not directly `RecursiveRules`, but instead just references to other rules in the grammar or a token kind to serve as base case.

| `RecursiveRule::` | Contains                                |
| ----------------- | --------------------------------------- |
| `Braced`          | Another rule surrounded by braces.      |
| `Concatenation`   | A concatenation of multiple rules.      |
| `Repetition`      | Repeats the same rule until a mismatch. |
| `Alternation`     | Finds the first rule that matches.      |

#### Concatenation Rules

A concatenation rule consists of at least one **essential** rule followed by any number of **required** rules:

- **Essential** rules will not cause an immediate error on mismatch and will instead revert back any advanced tokens from the token stream.
- **Required** rules will cause an error on mismatch but continue on with the next required rule.

Except for the final essential rule, all rules can be made optional:

1. `(Essential | Optional)*`
2. `Essential`
3. `(Required | Optional)*`

### Rule Builder

All named recursive rules also provide a way of creating a new `syntax_tree` node by combining existing tokens and node references in a `node_builder_input`.

## State

While recursion is a very powerful tool when it comes to writing a parser, I wanted to avoid using it. The reason being, that it makes it very easy to crash the host program with a script that contains e.g. a *lot* of nesting, causing a stack overflow. While there would have been ways around that by manually checking the parse depth or possibly even catching stack overflows, there just isn't a clean way.

Therefore, parsing is instead done with a stack of "parse requests" that are each processed without any recursive calls.

Some additional "global state" (global to the parsing process) is necessary as well:

| Name                 | Type                | Description                                          |
| -------------------- | ------------------- | ---------------------------------------------------- |
| `code`               | `&str`              | A string slice to the code to parse.                 |
| `grammar`            | `&Grammar`          | A reference to the grammar rules.                    |
| `token_streams`      | `Vec<TokenStream>`  | A stack of token streams.                            |
| `parse_requests`     | `ParseRequestStack` | A stack of pending parse requests.                   |
| `node_builder_input` | `NodeBuilderInput`  | A stack of tokens and references into `syntax_tree`. |
| `syntax_tree`        | `SyntaxTree`        | The final output.                                    |
| `diagnostics`        | `Vec<Diagnostic>`   | Keeps track of errors and other diagnostic messages. |

## Initial State

- `code` is the one and only input to the parsing process.
- `grammar` is basically just a big constant with all the parsing rules.
- `token_streams` contains a single token stream to the [first non-whitespace character](#initial-whitespace) of `code`.
- `parse_requests` contains a single request to root rule of the `grammar`.
- `node_builder_input` is empty.
- `syntax_tree` is empty.
- `diagnostics` is empty.

### Initial Whitespace

Any initial whitespace is skipped right before `token_streams` is constructed and stored in `syntax_tree`. Therefore any consecutive tokens queries will be a non-whitespace token followed by a whitespace query:

```txt
"     hello   world   "
'-----+++++---+++++---'
 |    |       '- token & trailing whitespace
 |    '- token & trailing whitespace
 '- initial whitespace
```

## Parsing Process

The parsing process consists of one single loop that pops a parse request from the stack and the processes it (without doing any recursive calls!). Once there are no more pending parse requests, parsing is finished.

## Parse Requests

There are multiple different types of parse requests:

| `ParseRequest::` | Description                                              |
| ---------------- | -------------------------------------------------------- |
| `Rule`           | A token or reference to a grammar rule.                  |
| `ClosingBrace`   | Used at the end of a braced rule.                        |
| `Build`          | Builds a `syntax_tree` node from `node_builder_input`.   |
| `Custom`         | Custom processing, for when the above is not sufficient. |

### `ParseRequest::Rule`

If it contains a token, tries to parse that token.

If it instead references a different grammar rule, pushes a `ParseRequest::Build` followed by additional `ParseRequest::Rule`s that make up the rule.

All rule parse requests also come with different `ParseMode`s:

| `ParseMode::` | Description                                            |
| ------------- | ------------------------------------------------------ |
| `Essential`   | Cancels the current `ParseRequest::Build` on mismatch. |
| `Required`    | Adds an error diagnostic but continues on mismatch.    |
| `Optional`    | Just continues on mismatch.                            |
| `Repetition`  | Repeatedly pushes back itself until mismatch.          |
| `Alternation` | Pops remaining alternations on mismatch.               |

#### Processing a build request for a `RecursiveRule::Braced`

Parses a nested token stream and then pushes a `ParseRequest::ClosingBrace` followed by a `ParseRequest::Rule` for what should be inside the braces.

#### Processing a build request for a `RecursiveRule::Concatenation`

Pushes more build requests for all contained rules in reverse order:

1. All required/optional rules in reverse order.
2. The single essential rule.
3. All essential/optional rules in reverse order.

Essential, required and optional rules are pushed with `ParseMode::Essential`, `ParseMode::Required` and `ParseMode::Optional` respectively.

#### Processing a build request for a `RecursiveRule::Repetition`

Pushes a build request with `ParseMode::Repetition`.

### `ParseRequest::ClosingBrace`

TODO

### `ParseRequest::Build`

Uses `node_builder_input` elements to construct a new `syntax_tree` node. The used elements of `node_builder_input` are replaced with a reference to the new node.

### `Custom`

TODO
