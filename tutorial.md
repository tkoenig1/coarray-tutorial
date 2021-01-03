# Introduction

Since the introduction of the Fortran 2008 standard, Fortran is a
parallel language.  Unlike the parallel extensions
[OpenMP](https://www.openmp.org/) or
[OpenACC](https://www.openacc.org/), the coarray parallelism
Coarrays is built into the language core, so there no
fewer problems with interaction between different standards and
different standards bodies.

This tutorial aims to introduce Fortran coarrays to the general user.
A general familiarity with modern Fortran is assumed.  People who are
not familar, but are familiar with other imperative languages like C
might need to refer to other sources such as the [FortranWiki](http://fortranwiki.org/fortran/show/HomePage) to check what
individual language constructs mean.

## What is the idea behind coarrays?

Coarrays follow the idea of a
[Partitioned global address space](https://en.wikipedia.org/wiki/Partitioned_global_address_space)
or PGAS.  In PGAS, there are several images executing. Each image has
its own local memory. It is, howewer, possible to access the
memory of other images via special constructs.

This is more loosely coupled than the thread model, where threads
share variables unless explicitly directed otherwise.

Using PGAS means that coarray Fortran can be used on a massively
massively parallel computing system as well as a shared-memory
implementation on a single, multi-CPU computer.

## A remark on compiling and running the example programs

If you want to try out the example programs, you need have a coarray-capable
compiler and know how to compile and run the programs. Setting the number
of images is done in a compiler-dependent manner, usually via a compiler option,
an environment variable, or, if the system is MPI-based, as an argument
to `mpirun`.

# Images and synchronization

One central concepts of coarray Fortran is that of an image.  When a
program is run, it starts multiple copies (or, possibly, one copy) of
itself. Each image runs in parallel until completion, and works
independently of other images unless the programmer specifically
asks for synchronization.

## A first example

Here is a Coarray variant of the classic "Hello world" program:
```
program main
  implicit none
  write (*,*) "Hello from image", this_image(), "of", num_images()
end program main
```
This program will output something like
```
 Hello from image           2 of           4
 Hello from image           4 of           4
 Hello from image           3 of           4
 Hello from image           1 of           4
```
depending on how many images you run and shows the use of two
important functions: The number of images that is run can be found
`num_images()` function and the current image via `this_image()`.
Both of these are functions that are built into the language
(so-called intrinsic functions).

## Basic Synchronization

Usually, some kind of ordering has to be imposed on the images to do
anything useful. This can be done with the `SYNC ALL` statement,
which partitions the programs into what the Fortran standard calls
segments.  Anything before one `SYNC ALL` statement will get executed
before anything after the `SYNC ALL` statement.

Here is an example program, where each image prints both a
Hello and a Goodbye message.  Assume you want to make sure
that each Goodbye message is printed before each Hello message,
then this is *not* the way to do it:
```
program main
  implicit none
  write (*,*) "Hello from image", this_image(), "of", num_images()
  write (*,*) "Goodbye from image", this_image(), "of", num_images()
end program main
```
The output will look something like
```
 Hello from image           4 of           4
 Goodbye from image           4 of           4
 Hello from image           3 of           4
 Goodbye from image           3 of           4
 Hello from image           1 of           4
 Hello from image           2 of           4
 Goodbye from image           1 of           4
 Goodbye from image           2 of           4
```
What you can do instead is to put things into order is to insert
`SYNC ALL` between the two `write` statements, like this:
```
program main
  implicit none
  write (*,*) "Hello from image", this_image(), "of", num_images()
  sync all
  write (*,*) "Goodbye from image", this_image(), "of", num_images()
end program main
```
which will get the intended result:
```
 Hello from image           2 of           4
 Hello from image           4 of           4
 Hello from image           3 of           4
 Hello from image           1 of           4
 Goodbye from image           1 of           4
 Goodbye from image           2 of           4
 Goodbye from image           4 of           4
 Goodbye from image           3 of           4
```

# Coarrays
In order to be really useful, the images need a way to exchange data
with other images.  This can be done with coarrays.

A coarray is just a normal variable, of any type, which can be either
a scalar or an array. Like for any other variable, there is one
instance for each image.  The variable itself can be a scalar or an
array.  A coarray has one important property: It is possible to
access data on another image, both for reading and writing, using
normal Fortran syntax.  Let us see how this works.

## Syntax of simple coarrays

Coarrays are declared either by using the `codimension` attribute or
by using square brackets in addition to normal brackets.  The final
codimension is unknown at compile-time (and can usually be selected
at run-time). This is expressed by using a `*` as the codimension.
The following declaration declares an integer coarray:
```
  integer :: a[*]
```
as does this line:
```
  integer, codimension[*] :: a
```

It is a matter of taste and line length which variant is used.
Accessing this coarray is done by putting the coindex in
square brackets.  For the simple case above, this is equal to the
value of `this_image()`. So, this statement prints the value of a on
image 5:

```
  integer :: a[*]
  print *,a[5]
```
and this sets the value of a on image 3 to 42:
```
  integer :: a[*]
  a[3] = 42
```
or you can even use I/O to set the value:
```
  integer :: a[*]
  read (*,*) a[3]
```
Of course, when these code fragments are run, the referenced image
has to exist.

## Simple use of coarrays
As previously mentioned, the images run independently unless
otherwise directed. The most important rule is that changes
to coarrays only get propagated to other images via synchronization.
So, for example, this fragment will *not* work as maybe expected:
```
  if (this_image() == 3) then
    a[2] = 42
  end if
  print *,a[2]
```
but this will:
```
  if (this_image() == 3) then
    a[2] = 42
  end if
  sync all
  print *,a[2]
```
You could access the variable `a` declared as above on its own image
by using `a[this_image(a)]`. While correct, there is a shortcut; you
can simply use `a` in that case.

So, here is a small example where image number 1 sums up the image
numbers, plus the expected value.  This uses a rather common idiom,
where all images do work, while only one of them does I/O.
```
program main
  implicit none
  integer :: sq[*]
  integer :: i, s, n
  sq = this_image()
  sync all ! Do not forget this.
  if (this_image() == 1) then
     s = 0
     n = num_images()
     do i=1, n
        s = s + sq[i]
     end do
     write (*,'(*(A,I0))') "Number of images: ", n, " sum: ", s, &
     	   " expected: ", n*(n+1)/2
  end if
end program main
```
With four images, this gives the result
```
Number of images: 4 sum: 10 expected: 10
```
Here is another example: A program where each image writes "Hello
from" and its own image number into a character coarray of the
image with `image_number()` one higher, or to 1 for the last
image number. Each image then prints out the greeting it received
from the other image.  Here is the program:
```
program main
  implicit none
  character (len=30) :: greetings[*]
  integer :: me, n, you
  me = this_image()
  n = num_images()
  if (me /= n) then
     you = me + 1
  else
     you = 1
  end if
  write (unit=greetings[you],fmt='(A,I0,A,I0)') &
       "Greetings from ", me, " to ", you
  sync all
  write (*,'(A)') trim(greetings)
end program main
```
and here its output with four images:
```
Greetings from 3 to 4         
Greetings from 1 to 2         
Greetings from 2 to 3         
Greetings from 4 to 1
```
## Coarrays as arrays
All examples so far have used coarrays which were scalars,
but they can be arrays, as well. A somewhat contrived example:
```
program main
  implicit none
  real, dimension(10) :: a[*]
  integer :: i
  call random_number(a)
  a = a**2
  sync all
  if (this_image () == num_images()) then
     do i=1,num_images()-1
        a = a + a(:)[i]
     end do
     print '(*(F8.5))',a
  end if
end program main
```
which will print the sum of the squares of 10 random numbers
for each image, something which could look like
```
 2.14682 2.70696 2.50518 3.09663 2.81545 1.88543 4.53160 2.67531 2.29398 2.96503
```
You will need the array reference `(:)` before the coarray
reference `[i]`, and you can use the full power of the
array indexing that Fortran provides.

## Lower cobounds not equal to one
If you feel like it, you can also set the lower bound of a
coarray to some other value. If you are a fan of C and like
zero lower bounds, the following is valid:
```
  integer :: a[0:*]
```
or if you are a fan of Douglas Adams, you can use
```
  integer :: a[42:*]
```
Actually, declaring a coarray a `a[*]` is only a shortcut for
declaring the coarray as `a[1:*]` with a lower cobound of 1.
There is a subtlety to the use of `this_image()`: Without
any arguments, it gives you the image number.  When it has
a coarray argument, it will give you the argument that you
need to access the coarray on the current image.
For example, in this program
```
program main
  integer :: a[42:*]
  print *, this_image(), this_image(a)
end program main
```
you will need a coindex of 42 to access the coarray on the first
image, and the program will print
```
           4          45
           2          43
           1          42
           3          44
```
## An example program
A classic example is the estimation of pi/4 by Monte Carlo
simulation. This program sets up the field n strips along
the x-axis, then distributes points randomly and checks if
they are inside or outside the unit circle.
```
program main
  implicit none
  integer, parameter :: blocks_per_image = 2**16
  integer, parameter :: block_size = 2**10
  real, dimension(block_size) :: x, y
  integer :: in_circle[*]
  integer :: i, n_circle, n_total
  real :: step, xfrom

  n_total = blocks_per_image * block_size * num_images()
  step = 1./real(num_images())
  xfrom = (this_image() - 1) * step
  in_circle = 0
  do i=1, blocks_per_image
     call random_number(x)
     call random_number(y)
     in_circle = in_circle + count((xfrom + step * x)** 2 + y**2 < 1.)
  end do
  sync all
  if (this_image() == 1) then
     n_circle = in_circle
     do i=2, num_images()
        n_circle = n_circle + in_circle[i]
     end do
     print *,"pi/4 is approximately", real(n_circle)/real(n_total), "exact", atan(1.)
  end if
end program main
```
## Multi-dimensional coarrays
It is also possible to have coarrays with more than one codimension.
This can be useful, for example, when using a computational grid.
The way to declare such a coarray is, for example,
```
  real :: a[2,*]
```
The asterisk is always the last codimension that needs to be
specified. If you have four images running, this declaration
will give you `a[1,1]`, `a[2,1]`, `a[1,2]` and
`a[2,2]`.

For coarrays with multiple codimension, `this_image()` will
give you all the indices for accessint the current image,
like this:
```
program main
  integer :: a[2,2:*]
  print *, this_image(), this_image(a)
end program main
```

What happens if the number of images is not divisible by two
in the above example?  The answer is complex, and it is
best to avoid this case for now.

## Allocatable coarrays
It is generally not considered enough to set the size of a problem
during compile-time.  Therefore, Fortran introduced allocatable
arrays, where the bounds can be set at run-time. This has also
ben extended to allocatable coarrays.  This is especially useful
if the coarrays hold a large amount of data.

An allocatable coarray can be declared with the syntax
```
  real, dimension(:), codimension(:), allocatable :: a
```
(note the colons in the declarations) and allocated with
```
  allocate (a(n)[*])
```
Like a regular allocatable variable, it will be deallocated
automatically when going out of scope. `SOURCE` and `MOLD`
can also be specified.

One important thing to notice is that coarray sizes have to
agree on all images, otherwise unpredictable things will happen;
at best, there will be an error message.  If you want to, you
can adjust the bounds.  This, for example, would be legal:
```
  from = (this_image() - 1) * n + 1
  to = this_image () * n
  allocate (a(from:to)[*])
```
and give you an index running from `1` to `num_images * n`, but
you would still have to specify the correct coarray.

# More advanced synchronization -- `SYNC IMAGES`

`SYNC ALL` is not everything that may be needed for synchronization.
Suppose not every image needs to communicate with every other image,
but only with a specific set.  It is possible to use `SYNC IMAGES`
for this purpose.

`SYNC IMAGES` takes as argument an image, or a list of the images
with which it should synchronize, for example
```
  if (this_image () == 2) sync_images ([1,3])
```
This will hold execution of image number two until a corresponding
`SYNC IMAGES` statement has been executed on images 1 and 3:
```
  if (this_image () == 1) sync_images (2)
  if (this_image () == 3) sync_images (2)
```
The following example uses `SYNC IMAGES` for a pairwise exchange of
greetings between different images:
```
program main
  implicit none
  character (len=30) :: greetings[*]
  integer :: me, n, you
  me = this_image()
  n = num_images()
  if (mod(n,2) == 1 .and. me == n) then
     greetings = "Hello, myself"
  else
     you = me + 2 * modulo(me,2) - 1
     write (unit=greetings[you],fmt='(A,I0,A,I0)') &
          "Greetings from ", me, " to ", you
     sync images (you)
  end if
  write (*,'(A)') trim(greetings)
end program main
```
Here is an idiom to have image 1 prepare something and
have all images wait on image 1, plus have image 1
wait on all other images:
```
program main
  implicit none
  if (this_image() == 1) then
     write (*,'(A)') "Preparing things on image 1"
     sync images(*)
  else
     sync images(1)
  end if
  write (*,'(A,I0)') "Using prepared things on image ", this_image()
end program
```
Two images can issue `SYNC IMAGES` commands to each other multiple
times. Execution will only continue if the numbers match.

# Coroutines

Another method.

# Getting it to work

## Using gfortran

At the time of writing of this document, this may unfortunately
be challenging.
The [GNU Fortran compiler](https://gcc.gnu.org/onlinedocs/gfortran/)
two possiblitites: Use of the `-fcoarray=lib` option and using
[OpenCoarrays](http://www.opencoarrays.org/).  You may also need
to have MPI installed to run it.

Another possibilility currently under development is the [shared
memory coarray
branch](https://gcc.gnu.org/git/?p=gcc.git;a=tree;h=refs/heads/devel/coarray_native;hb=refs/heads/devel/coarray_native).
This is currently under active development, but does not yet have all
features implemented.

## Using ifort

Refer to the [ifort documentation](https://software.intel.com/content/www/us/en/develop/documentation/fortran-compiler-oneapi-dev-guide-and-reference/top.html) 
for details.

## Using NAG Fortran

Refer to the [NAG Fortran compiler documentation](https://www.nag.com/content/nag-fortran-compiler) for details.


