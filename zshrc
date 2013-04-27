ZSH=$HOME/.zsh
ZSH_PLUGINS=/usr/share/zsh/plugings

for config_file in $ZSH/*.zsh
do
    source $config_file
done

if [[ -f .secrets ]]
then
    source .secrets
fi

ZSH_SYNTAX=$ZSH_PLUGINGS/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
if [[ -f $ZSH_SYNTAX ]]
then
    source $ZSH_SYNTAX
fi

PYTHONZ=$HOME/.pythonz/etc/bashrc
if [[ -f $PYTHONZ ]]
then
    source $PYTHONZ
fi
