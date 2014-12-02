df1 = data.frame(CustomerId=c(1:6),Product=c(rep("Toaster",3),rep("Radio",3)))
df2 = data.frame(CustomerId=c(2,4,6),State=c(rep("Alabama",2),rep("Ohio",1)))

df1
df2

outer <- merge(x = df1, y = df2, by = "CustomerId", all = TRUE)
outer
left.outer <- merge(x = df1, y = df2, by = "CustomerId", all.x=TRUE)
left.outer

right.outer <- merge(x = df1, y = df2, by = "CustomerId", all.y=TRUE)
right.outer
cross.join <- merge(x = df1, y = df2, by = NULL)
cross.join