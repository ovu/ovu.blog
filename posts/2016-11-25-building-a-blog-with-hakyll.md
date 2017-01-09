---
title: Building a blog with Hakyll
author: Omar Diego Vera Ustariz
description: Introduction to Hakyll, a tool for generating static websites. It introduces the basics of Hakyll and describes the first steps to start using it. It presents tips and tricks that you should know in order to start using it successfully.
---

I am really happy to start writing and even more to write something about Hakyll.

I was looking for a tool to generate static Html content to publish my blog and I found some alternatives,
I think the most famous is Jekyll. It is a tool written in ruby that does exactly that, it generates static content.
However, I found a really good alternative called Hakyll, which is a tool written in Haskell that offers a DSL 
to define rules to generate Html. 

I decided to use Hakyll because I wanted to practice some Haskell and to use as well a serious project written in that language.

My goal is to write my posts in markdown, transform them into Html and to host the content in Github.
These teps are what I am going to describe in this post.

#### Installation of Hakyll

Hakyll has a good documentation about the installation and first steps. Its website contains some [tutorials](https://jaspervdj.be/hakyll/tutorials.html). I will recommend to read them for the installation and understanding Hakyll.

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
The file site.hs contains the rules to convert the markdown files into Html. The rules are described in a DSL written in Haskell. In the tutorial I mentioned above you will find enough information about the rules and how to customize the site.hs according to your needs.

The generated files are a good starting point to for a blog website. I started modifying it in order to get the version I have now published.

In order to build the executable of the site.hs run the following command:

``` bash
$ stack build
```

#### Serving and watching files

Something I found really useful was that Hakyll provides a server that can be used to see how the generated pages will see when they are hosted. Not just that, Hakyll has a watch option that checks for changes in the content and generates the Html and host them immediately.

To host the generated files and watch them while changing just execute the following command:

``` bash
$ stack exec site watch
```

#### Bonus: publishing in Github pages
By default, Hakyll generates the Html files and css into a directory named _site. Github pages allow publishing in the root of a repository or inside a docs directory. Note that according to the [Github pages](https://help.github.com/articles/configuring-a-publishing-source-for-github-pages/) the name of the repository should not have the format <Github user name>.github.io.

Hakyll is so easy to configure that it can be done in a simple way. Add a configuration variable in the file site.hs
``` haskell
myConfiguration:: Configuration
myConfiguration = defaultConfiguration 
                  {
                    destinationDirectory = "docs"
                  }
```

The code above overwrites the default destination directory of Hakyll. Then we can use the configuration as follows:
 
``` haskell
main :: IO ()
main = hakyllWith myConfiguration $ do
       -- Here should come your rules
```
Do not forget to configure the Github pages to use the docs directory. And voila! You should have already you website.

#### Conclusion
Hakyll is a really good documented tool that can be used to generate static pages. It has a very intuitive DSL for defining the generation rules.
At the beginning, I mentioned I wanted to use it in order to practice Haskell. However, it is not necessary to program in Haskell to have it running, at least in the beginning.

Publishing the generated pages in Github were easy as well. I will continue adding features to the blog and maybe I will discover more features of Hakyll and learn more about it.
