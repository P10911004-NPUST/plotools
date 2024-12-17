greek_letter <- function(
        symbol_name = c(
            "alpha", "beta", "gamma", "delta", "epsilon", 
            "zeta", "eta", "theta", "iota", "kappa", 
            "lambda", "mu", "nu", "xi", "omicron",
            "pi", "rho", "sigmaf", "sigma", "tau",
            "upsilon", "phi", "chi", "psi", "omega"
        ),
        format = c("unicode", "html"),
        show_lower_case = TRUE
){
    symbol_name <- match.arg(symbol_name)
    format <- match.arg(format)
    output <- NULL
    
    # left is lower case, right is capital; i.e. c(lower-case, upper-case)
    letter_list <- list(
        
        ## Unicode (UTF-16) ==================
        # https://www.compart.com/en/unicode
        unicode = list(
            alpha      = c("\U03B1", "\U0391"),
            beta       = c("\U03B2", "\U0392"),
            gamma      = c("\U03B3", "\U0393"),
            delta      = c("\U03B4", "\U0394"),
            epsilon    = c("\U03B5", "\U0395"),
            zeta       = c("\U03B6", "\U0396"),
            eta        = c("\U03B7", "\U0397"),
            theta      = c("\U03B8", "\U0398"),
            iota       = c("\U03B9", "\U0399"),
            kappa      = c("\U03BA", "\U039A"),
            lambda     = c("\U03BB", "\U039B"),
            mu         = c("\U03BC", "\U039C"),
            nu         = c("\U03BD", "\U039D"),
            # xi         = c("\U03BE", "\U039E"),
            # omicron    = c("\U03BF", "\U039F"),
            # pi         = c("\U03BG", "\U039G"),
            # rho        = c("\U03BH", "\U039H"),
            # sigmaf     = c("\U03BI", "\U039I"),
            # sigma      = c("\U03BJ", "\U039J"),
            # tau        = c("\U03BK", "\U039K"),
            # upsilon    = c("\U03BL", "\U039L"),
            # phi        = c("\U03BM", "\U039M"),
            # chi        = c("\U03BN", "\U039N"),
            # psi        = c("\U03BO", "\U039O"),
            omega      = c("\U03C9;", "\U03A9")
        ),
        ## HTML entity (decimal) ========================
        # https://www.freeformatter.com/html-entities.html
        html = list(
            alpha      = c("&#945;", "&#913;"),
            beta       = c("&#946;", "&#914;"),
            gamma      = c("&#947;", "&#915;"),
            delta      = c("&#948;", "&#916;"),
            epsilon    = c("&#949;", "&#917;"),
            zeta       = c("&#950;", "&#918;"),
            eta        = c("&#951;", "&#919;"),
            theta      = c("&#952;", "&#920;"),
            iota       = c("&#953;", "&#921;"),
            kappa      = c("&#954;", "&#922;"),
            lambda     = c("&#955;", "&#923;"),
            mu         = c("&#956;", "&#924;"),
            nu         = c("&#957;", "&#925;"),
            xi         = c("&#958;", "&#926;"),
            omicron    = c("&#959;", "&#927;"),
            pi         = c("&#960;", "&#928;"),
            rho        = c("&#961;", "&#929;"),
            sigmaf     = c("&#962;", "&#930;"),
            sigma      = c("&#963;", "&#931;"),
            tau        = c("&#964;", "&#932;"),
            upsilon    = c("&#965;", "&#933;"),
            phi        = c("&#966;", "&#934;"),
            chi        = c("&#967;", "&#935;"),
            psi        = c("&#968;", "&#936;"),
            omega      = c("&#969;", "&#937;")
        )
        
    )
    
    output <- letter_list[[format]][[symbol_name]]
    output <- ifelse(show_lower_case, output[1], output[2])
    
    return(output)
}

