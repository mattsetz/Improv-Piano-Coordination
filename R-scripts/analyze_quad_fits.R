inDir = 'Analysis/jags/quad_fits_no_thin'
files = list.files(inDir)

fits <- NULL
ess <- NULL
for (f in files) {
  print(f)
  samples_df <- as.data.frame(as.matrix(get(load(paste(inDir,f,sep='/'))),chains = TRUE))
  samples_df$ntime=as.numeric(str_extract_all(f,"[0-9]{1,3}")[[1]][[1]])
  samples_df$window_size=as.numeric(str_extract_all(f,"[0-9]{1,3}")[[1]][[2]])
  fits <- rbind(fits,samples_df)
}

fits %>% group_by(window_size,ntime) %>% count()

# is Effective Sample Size influenced by ntime or window_size?
ggplot(ess_df, aes())

fits <- fits %>% filter(window_size!=0) # don't understand this, oh well

fits %>% group_by(window_size,ntime) %>% count()
fits.w <- fits %>% filter(window_size==2)
ggplot(fits,aes(x=beta1_cond1_cond2))+geom_histogram()+facet_wrap(~ntime+window_size)
colnames(fits.w)

" How do we interpret more negative linear coefficient in coupled trials?"
quad <- NULL
for (B in -seq(0,100,30)) {
  x=-100:100
  y=x^2+B*x
  quad <- rbind(quad,data.frame(B=B,x=x,y=y))
}
ggplot(quad,aes(x=x,y=y))+geom_point()+facet_wrap(~B)

"How do we interpret larger x^2 coefficient in coupled trials?"
quad <- NULL
for (A in 1:5) {
  x=-100:100
  y=A*x^2+x
  quad <- rbind(quad,data.frame(A=A,x=x,y=y))
}
ggplot(quad,aes(x=x,y=y))+geom_point()+facet_wrap(~A)
"
Bigger quad coeffients mean steeper curves.
"