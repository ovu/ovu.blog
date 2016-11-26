---
title: Hakyll first steps
author: Omar Diego Vera Ustariz
description: Introduction to Hakyll, a tool for generating static websites. It introduces the basics of Hakyll and describes the first steps to start using it. It presents tips and tricks that you should know in order to start using it successfully.
---

I am really happy to start writing and even more to write something about Hakyll.

I was looking for a tool to generate static html content to publish my blog and I found some alternatives,
I think the most famous is jekyll. It is a tool written in ruby that does exactly that, it generates static content.
However, I found a really good alternative called Hakyll, which is a a tool written in haskell that offers a DSL 
to define rules to generate html. 

I decided to use Hakyll because I wanted to practice some haskell and to use as well a serious project written in that language.

My goal is to write my posts in markdown and to transform the files into html and to host the content in github.
This first steps is what I am going to describe in this post.

#### Installation of Hakyll

Hakyll has a good documentation about the installation and first steps. In the website there are some [tutorials](https://jaspervdj.be/hakyll/tutorials.html). I will recommend to read them for the installation and understanding Hakyll.

#### Creating the initial structure of the blog

Hakyll has a really easy scaffolding feature. Inside the directory where the pages will be generated run the following command:
``` bash
$ hakyll init
```

After executing the command the following structure will be generated:
``` bash
$ tree
.
├── about.rst
├── contact.markdown
├── css
│   └── default.css
├── images
│   └── haskell-logo.png
├── index.html
├── posts
│   ├── 2012-08-12-spqr.markdown
│   ├── 2012-10-07-rosa-rosa-rosam.markdown
│   ├── 2012-11-28-carpe-diem.markdown
│   └── 2012-12-07-tu-quoque.markdown
├── site.hs
└── templates
      ├── archive.html
      ├── default.html
      ├── post-item.html
      └── post.html
```
The file site.hs contains the rules to convert the markdown files into html. The rules are described in a DSL written in haskell. In the tutorial I mentioned above you will find enough information about the rules and how to customize the site.hs according to your needs.

The generated files are a good starting point to for a blog website. I started modifying it in order to get the version I have now published.

In order to build the executable of the site.hs run the following command:

``` bash
$ stack build
```

#### Serving and watching files

Something I found really useful was that Hakyll provides a server that can be used to see how the generated pages will see when they are hosted. Not just that, Hakyll has a watch option that check for changes in the content and generate the html and host them immediately.

To host the generated files and watch them while changing just execute the following command:

``` bash
$ stack exec site watch
```

#### Bonus: publishing in github pages
By default Hakyll generates the html files and css into a directory named _site. Github pages allow publishing in the root of a repository with the name <github name>.github.io or inside a docs directory inside that repository.

Hakyll is so easy to configure that it can be done in a sample way. Add a configuration variable in the file site.hs
``` haskell
myConfiguration:: Configuration
myConfiguration = defaultConfiguration 
                  {
                    destinationDirectory = "docs"
                  }
```

The code above overwrites the default destination direcory of Hakyll. Then we can use the configuration as follows:
 
``` haskell
main :: IO ()
main = hakyllWith myConfiguration $ do
       -- Here should come your rules
```
Do not forget to configure the github pages to use the docs directory. And voila! You should have already you website.

#### Conclusion
Hakyll is a really good documented tool that can be used to generate static pages. It has a very intuitive DSL for defining the generation rules.
At the beginning I mentioned I wanted to use it in order to practice Hakell, however it is not necessary to program in Haskell to have it running, at least in the first steps.

Publishing the generated pages in github were easy as well. I will continue adding features to the blog and maybe I will discover more features of Hakyll and learn more about it.
