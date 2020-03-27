function create
  if test ! -e "$argv.fsx"
    cp lib/prelude.fsx "$argv.fsx"
  end
end

function clear_test
  cat /dev/null > "test"
end