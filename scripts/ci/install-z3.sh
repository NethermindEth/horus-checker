sudo apt-get remove libz3-dev
curl -L -O https://github.com/Z3Prover/z3/releases/download/z3-4.8.9/z3-4.8.9-x64-ubuntu-16.04.zip
unzip z3-4.8.9-x64-ubuntu-16.04.zip
sudo cp z3-4.8.9-x64-ubuntu-16.04/bin/libz3.so /usr/lib/libz3.so
sudo cp z3-4.8.9-x64-ubuntu-16.04/include/* /usr/include/
sudo cp z3-4.8.9-x64-ubuntu-16.04/bin/z3 /usr/bin/z3
