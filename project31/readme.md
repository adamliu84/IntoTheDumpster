# Compile/Link/Run cmd

```
g++ -c cpp_library.cpp -o cpp_library.o
g++ -shared -o libcpp_library.so cpp_library.o
ghc haskell_prog.hs -lcpp_library -L.
export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:.
./haskell_prog
```

> The linker interprets the command-line flag -l as a shorthand for linking a library. When you provide the argument cpp_library to the -l flag (as in -lcpp_library), the linker automatically prefixes it with lib and appends a standard library extension like .so (for shared objects) or .a (for static archives) to create the full filename.
