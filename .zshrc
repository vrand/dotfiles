ZSH=$HOME/.zsh
 
for config_file in $ZSH/*.zsh 
do
    source $config_file
done
