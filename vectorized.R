result  = T & F #F
result  = T && F #F

result  = c(T, T) && c(F, F) #Error
result  = c(T, T) && c(F, F) # Error
result  = c(T, T) & c(F, F) # FF
result  = c(T, T) & c(T, F) # TF
result  = c(T, T) & c(T, T) # TT

result  = c(T, T, T) & c(F, F) # FFF Warning
result  = c(T, T, T, T) & c(F, F) # FFFF NO Warning


result  = T | F #T
result  = T || F #T
result  = c(T, T) | c(F, F) # TT
result  = c(T, T) || c(F, F) # Error
result  = c(T, T, T) | c(F, F) # TTT Warning
result  = c(T, T, T, T) | c(F, F) # TTTT NO Warning

# Summary
# single & | are vectorized and perform elementwise comparison
# should be even lengthed vectors, otherwise uses recycling
# in practice, never use %% and ||