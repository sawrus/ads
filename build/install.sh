sudo apt-get install erlang
erl -version
sudo apt-get install git-core
git --version
git clone git://github.com/basho/rebar.git
cd rebar
./bootstrap
echo "export PATH=$PWD:$PATH" >> $HOME/.bashrc
rebar --version
sudo apt-get install redis-server
redis-cli set x 0
redis-cli get x
mkdir rel
cd rel/
rebar create-node nodeid=ads
cd ..
rebar get-deps
rebar compile
