# Reprojuicer

Squeeze your R dataset into shape, reproducibly. 

![](assets/Juice1.png)

## Installation

>library(devtools)
>install_github("stevepowell99/reprojuicer")

You will also need to have a java install because this package relies heavily on XLConnect.

## One-minute intro

Reprojuicer is most useful where you have a R dataset which you need to clean up, organise and most importantly label for further reporting and analysis; and you want to do this *reproducibily*. 

Reprojuicer generates a simple .xls file corresponding to any `data.frame` you give it, ... 

just type 

>reprojuice("mtcars")

This will create, and should also open, a spreadsheet which gives you an overview of the variables in your dataset - a bit like Variable Viewer in SPSS but much more powerful. You use this spreadsheet to add labels, recodes, formulae and organise variables into blocks. You can drag and drop and reorder and use `fill series` and add colours and comments and all the other conveniences of a spreadsheet.

For example, you can:

- give your variables new *names* by typing in the *newvarname* column
- give them labels by typing in the *label* column
- apply a formula to your variable by typing, say, `factor(cyl)` in the `formula` column for the variable `cyl`
- create a new row, give it a name in the varname column and give it a formula of `cyl*2` in the formula column
- define one or more blocks by typing the name of the block in the `block` column. So you could type "x" for the first three variables to make a block containing these variables, and "y" for some of the other variables to define another block called `y`.  

Then just save the spreadsheet and run 

>reprojuice("mtcars")

again to create a new dataset, `mtcars.r` in this case, which is the result of systematically and reproducibly applying the transformations implied in your spreadsheet to the raw data.

So given a spreadsheet defining the transformations you want, reprojuicer loads up your environment with a transformed dataset `old_data_set_name.r`.

View the resulting data with, for example,

>str(mtcars.r)

Reprojuice also creates a data.frame for each block of variables you created, for easy access.  So if you defined a block called x by typing "x" into one or more rows of the `block` column, you can access it with

>b$x


There is also a function
`ggplotl()` which is just `ggplot()` except that it uses variable labels rather than variable names when labelling axes, fills, facets etc.


## Why reprojuicer? - Labels, convenience and reproducibility

Variable labels are particularly important in social science research, for example where we need to report answers to survey questions and the actual text of the question is important. 

Sometimes you need to repeatedly switch between long and short labels. Or you need to switch between labels in different languages.

Typically with surveys, you also need to group your variables into blocks, whereby sometimes each variable belongs to only one block, for example *answers to a grid of questions about political preferences* or in other cases you may (also) need to group each variable into more than one block.

What is more, you often want to do a set of recodes to all the variables in one or more blocks, for example to recode 99 as NA (missing data), or to group responses to an open question into a smaller set of responses.

You probably also need to do some other transformations on individual variables and/or blocks of variables (into existing or new variables or blocks) . 

The big temptation is to do these things *non-reproducibily*. You might try to work reproducibly in your actual analysis, but it is hard to resist the convenience of undocumented, manual hacking away at variable labels etc directly in the original database, whether it is Excel or SPSS or whatever.

So after a few months you have no idea exactly what you did, and when you have to repeat the survey you have to do all your hacking all over again.

It isn't too hard for advanced users to do this in an R script, but reprojuicer lets you do all of the above using a simple spreadsheet interface. The spreadsheet interface also helps you get an overview of longer and more complex datasets with potentially dozens of blocks and sub-blocks.






## More details


### Using the spreadsheet e.g. prop.mtcars.xls

You should be able to style it, i.e. change colours and column widths, and they should be retained.

Remember to save your spreadsheet file before running reprojuicer! reprojuicer should then close and reopen your file, possibly changed by the script.

The recoded data is saved under the same name but with .r at the end, e.g. data.r – so you will be writing mydata.r$myvariable or mydata.r[,"myvariable"]

You can drag and drop the rows to reorder the data, just leave the header row alone!

You can drag and drop the columns if you want a different order, just leave the first column alone!

If you add and/or delete columns in your raw data file and they get automagically updated in the props file after you run reprojuice(“nameOfDatase”)

Each column is 



### Recodes ...

Recoding one or more variables can be a bit tricky in R, especially for factors. It is also awkward to do recodes in a normal R script when they involve a lot of text. Recoding an open-ended variable which might have dozens or hundreds of responses is really a nightmare in a script interface. So the temptation is to open the original data file, add a column for the recoded variable, and do it by hand. But, the reproducible scientist never ever opens the original data file! Anyway, can you be sure you always recode different occurrences of "not bothered" as "indifferent"?

With reprojuicer, if you put text xyz in the recode column, a recode tab called xyz will be added after you run wprop. You can put any text you want (but as usual it is probably better to stick to latin letters and no funny symbols). If you put the same code more than once, a single tab will be opened with the combined values of those variables and you can recode them all at the same time.
You can put several different inputs in successive cells, i.e. in the input column and in the unnamed columns to the right of it.

In most cases, values not included in the input columns will be unchanged. But in the case of factors, they will be converted to NAs. 
You can put the same recode tag in the recode column for different variables and all will be recoded in the same way.

Let's take a simple case - you want to recode `0` and `1` as `male` and `female` in the variable `gender`. 

Just type a name for your recode, say `gender`, but it could be anything, in the recode column in the row for the gender variable in your reprojuicer spreadsheet. Run `reprojuice("mydata")` as usual. Depending on your spreadsheet software, you might need to refresh the spreadsheet (in Libreoffice / Openoffice this is File/Reload). You should see a new tab called gender with the levels of the variable (0 and 1) in the rows of two columns, output and input. So to recode, you just change the values in the output column to whatever you want, say male and female.

Run `reprojuice("mydata")` one more time and inspect mydata.r$gender. 

Additional features of recoding: 

- you can easily recode many variables at the same time by just typing the same name in the recode column for the variables you want to recode
- you can use `NA` for missing input and output values
- you can drag the rows in the recode tab around to reorder the levels of the variable
- importantly, you can recode more than one input value to one output value by stacking them up to the right, like this:

Output        | Input |  |  | 
------------- | -------------| -----| -----| -----
good          | excellent | quite good | good | OK
bad            | bad | dreadful | awful |

So when you have a variable with lots of values you want to recode into a smaller set of values, the input and output columns will initially have lots of rows. Type the few output values you want in the output column and delete the rest. Then you just have to drag the values from the input column into the corresponding rows.

#### NAs, empty strings (“blanks”) etc

It depends on the output format set by setlevout. For numeric vars, blank cells will be treated as NAs, for character and factor vars they will be treated as blanks. If you want a blank in the input spec it has to be in first column (otherwise the script doesn’t know where to stop looking...)
you can also type NA in input or output columns. So for numeric vars, i.e. if setlevout is num or int, NA is the same as blank.

TODO: You can put e.g. 4:8 or lo:3 or 4:hi or else in the column labelled “Input”.


### Formula column

You can add variables just by adding a new row in the table and AT THE SAME TIME define them in the formula column e.g. just put cyl to duplicate cyl. Should be able to put anything which would work in the script itself after an equals sign.

The expression is evaluated in the context of the new data frame up to the current row, and the old data frame after the current row.

So if you have already defined or redefined some variable x in the rows above, then just putting x will result in a complete copy of that variable. 

The variables are constructed in this order: starting from the original variable (if it exists, if not it is filled with NAs), then a formula (which may overwrite the previous contents completely, and which may refer to the existing data or to other variables from the original datafile (if you give the full names) or to variables from the new data file (you don’t need the data.frame prefix)), and then recodes, and then setlevout.

### newvarnames column: Renaming variables

You can use the column newvarnames to provide new variable names for those variables. The variables are renamed last, so if you are using formulas you have to refer to the old names.

### Setlevout column

Any variables set using setlevout get transformed correspondingly. They are actually transformed, i.e. their class is changed in R.

This is used just to specify the level of any recoded variables, after the recode. 

Possible values of setlevout

- str=character (i.e. class=character)
- nom=nominal i.e. unordered factor
- ord=ordinal i.e. unordered factor
- int=integer (i.e. class=integer)
- con=continuous (i.e. class=numeric)  (these last two are not much different)
- log=logical


### The block column: Defining blocks


### Other columns and the Findr function

You can add any columns in the spreadsheet you want e.g. mytag, and fill it with anything you want, mainly to make it easy to select different blocks. 

Then findr[mydata,"mytag"] is the block (assuming you used “x" to mark your vars; if e.g. you used y, do findr(mydata,"mytag","y"). Note that findr will find x or y if it is anywhere in the cell. So you can use findr just the same to identify variables by their varnames, e.g. findr(dat=mydata,"varnames","-") to identify all the variables with – anywhere in their varnames.


## Vignette

When I write it, you can run browseVignettes("reprojuicer") or try [this](vignettes/reprojuicer.Rmd)
