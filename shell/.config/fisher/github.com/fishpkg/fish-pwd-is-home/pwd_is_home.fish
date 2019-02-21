function pwd_is_home
    switch "$PWD"
        case ~{,/\*}
        case \*
          return 1
    end
end
