function fc
    set -l f (mktemp)
    history | head -n1 > $f
    vim $f
    and cat $f
    and source $f
    rm $f
end
