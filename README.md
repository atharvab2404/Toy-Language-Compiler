# Toy-Language-Compiler

Steps to make an run the frontendex:  
- Place frontend.cpp in llvm-project/build/bin  
- open the terminal in the same directory  
- execute: clang++ -g frontendex.cpp -o frontendex -g -O3 `llvm-config --cxxflags --ldflags --system-libs --libs core mcjit native`
- execute: ./frontendex "toy language code file name to be executed"  
