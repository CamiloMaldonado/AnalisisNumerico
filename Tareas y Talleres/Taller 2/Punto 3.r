#Construya un polinomio del menor grado que interpole una función f(x) en los siguientes datos: f(1) = 2; f(2) = 6; f 0 (1) = 3; f 0 (2) = 7; f 00(2) = 8
`` {r}
require ( pracma )
# Se limpian los elementos creados con anterioridad
rm ( lista = ls ())
gato ( " \ 0 14 " )
split.differences  <-  función ( x , y , x0 ) {
  require ( rSymPy )
  n  <- longitud ( x )
  q  <-  matriz ( datos  =  0 , n , n )
  q [, 1 ] <-  y
  f  <- como.caracter (round ( q [ 1 , 1 ], 5 ))
  fi  <-  ' '
  
  para ( i  en  2 : n ) {
    para ( j  en  i : n ) {
      q [ j , i ] <- ( q [ j , i - 1 ] -  q [ j - 1 , i - 1 ]) / ( x [ j ] -  x [ j - i + 1 ])
    }
    fi  <- paste ( fi , ' * (x - ' , x [ i - 1 ], ' ) ' , sep  =  ' ' , collapse  =  ' ' )
    
    f  <- paste ( f , ' + ' , round ( q [ i , i ], 5 ), fi , sep  =  ' ' , collapse  =  ' ' )
  }
  
  x  <- Var ( ' x ' )
  sympy (paste ( ' e = ' , f , collapse  =  ' ' , sep  =  ' ' ))
  aproximadamente  <- sympy (pegar ( ' e.subs (x, ' , as.character ( x0 ), ' ) ' , sep  =  ' ' , collapse  =  ' ' ))
  
  return ( list ( ' Aproximación de la interpolación ' = as.numeric ( aprox ),
              ' Función interpolada ' = f ,
              ' Tabla de diferencias divididas ' = q ))
}
x  = c ( 0 , 1 , 2 )
y  = c ( 10 , 15 , 5 )
resultados  <- divide.differences ( x , y , 1 )
print ( resultados $ ` Función interpolada`  )
`` `
