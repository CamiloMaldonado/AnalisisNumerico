#Camilo Maldonado, Pablo Veintemilla, Jose Zuluaga y Sergio Peñaranda
`` {r}
requerir ( pracma )
# Se limpian los elementos creados con anterioridad
rm ( lista = ls ())
gato ( " \ 0 14 " )
f  <-  función ( x ) {
  log ( x )
}
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
xs  = seq ( 1 , 2 )
y  = f ( xs )
resultados  <- divide.differences ( xs , y , 1 )
resultados2  <- divide.differences ( xs , y , 2 )
cat ( " Ln (x) en x = 1 \ n " )
print ( resultados $ ` Aproximación  de la  interpolación` )
print ( resultados $ ` Función interpolada`  )
print ( resultados $ ` Tabla de diferencias divididas`  ) 
cat ( " Ln (x) en x = 2 \ n " )
print ( resultados2 $ ` Aproximación  de la  interpolación` )
print ( resultados2 $ ` Función interpolada`  )
print ( resultados2 $ ` Tabla de diferencias divididas`  ) 
`` `
