# Brainfuck Interpreters in OCaml

This repository contains two different Brainfuck interpreters written in OCaml:

1. A **Naive Brainfuck Interpreter** that simply follows the rules of the language directly.
2. An **Optimized Brainfuck Interpreter** that includes intermediate representation and optimizations.

## 1. Naive Brainfuck Interpreter

The **Naive Brainfuck Interpreter** executes the Brainfuck program directly according to the rules without any optimization or intermediate representation. It is simple to understand but inefficient.

## 2. Optimized Brainfuck Interpreter with Intermediate Representation

The **Optimized Brainfuck Interpreter** works by first converting the Brainfuck program into an intermediate representation, which is then executed. This representation reduces redundant operations (such as set a cell to a certain value), resulting in better performance.

## How to Run

You can run the interpreter by either providing a filename with the Brainfuck code:

```bash
./interpreter filename.bf
```

or by passing the Brainfuck source code directly as a command-line argument:

```bash
./interpreter "source_code"
```
