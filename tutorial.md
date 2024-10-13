# Introduction

Since the introduction of the Fortran 2008 standard, Fortran is a
parallel language.  Unlike the parallel extensions
[OpenMP](https://www.openmp.org/) or
[OpenACC](https://www.openacc.org/), the coarray parallelism
Coarrays is built into the language core, so there are
fewer problems with interaction between different standards and
different standards bodies.

This tutorial aims to introduce Fortran coarrays to the general user.
A general familiarity with modern Fortran is assumed.  People who are
not familar with Fortran, but are familiar with other imperative languages like C
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

Using PGAS means that coarray Fortran can be used on a
massively parallel computing system as well as a shared-memory
implementation on a single, multi-CPU computer.

## A remark on compiling and running the example programs

If you want to try out the example programs, you need to have a coarray-capable
compiler and know how to compile and run the programs. Setting the number
of images is done in a compiler-dependent manner, usually via a compiler option,
an environment variable, or, if the system is MPI-based, as an argument
to `mpirun`.

# Images and synchronization

One central concept of coarray Fortran is that of an image.  When a
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
with the
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
What you can do instead to put things into order is to insert
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
The `SYNC ALL` statements do not have to be in the same place in the
program.  For example, this program will print the "Hello" message
from image 1 later than all the others:
```
program main
  implicit none
  if (this_image() == 1) sync all
  write (*,*) "Hello from image", this_image()
  if (this_image() /= 1) sync all
end program
```
Output is (for example)
```
 Hello from image           2
 Hello from image           4
 Hello from image           3
 Hello from image           1
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
  integer :: me[*]
  integer :: i, s, n
  me = this_image()
  sync all ! Do not forget this.
  if (this_image() == 1) then
     s = 0
     n = num_images()
     do i=1, n
        s = s + me[i]
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
you would still have to specify the correct coindices.

`ALLOCATE` and `DEALLOCATE` also do implicit synchronization,
so you can use the allocated coarrays directly, no need to
specifcy any `SYNC` variant.

# More advanced synchronization

`SYNC ALL` is not everything that may be needed for synchronization,
Fortran allows for more fine-grained control.

## `SYNC IMAGES`

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

A slightly more complex example.  Assume you want to write "Hello,
world" from each image in reverse sequence (because you can).  Here
is a program to do this:

```
program main
  implicit none
  integer :: me
  me = this_image()
  if (me < num_images()) sync images(me + 1)
  print *,"Hello, world from", this_image()
  if (me > 1) sync images (me - 1)
end program main
```

Let's look at what happens with this program: All images but the one
with the highest number wait until the image with one number higher
has synchronized with them, so they get stuck (temporarily) in the
first `SYNC IMAGES` statement.  The image with the highest number
does not execute that, but runs straight through to the print statement
and synchronizes with the one below, which then runs executes the
print statement, which... until `me = 1`.

Output could look like
```
 Hello, world from           4
 Hello, world from           3
 Hello, world from           2
 Hello, world from           1
```
## `CRITICAL` and `END CRITICAL`

Sometimes, it is desirable to protect some resource from interference
from other images.  This can be done via the `CRITICAL` and `END
CRITICAL` statements.

The syntax is simple:
```
  CRITICAL
    ! Only one image may execute this part at a time
  END CRITICAL
```

## LOCK and UNLOCK

Whie ```CRITICAL``` allows for some protection, pepole might want
something more fine-grained.  For this, there is the `LOCK_TYPE` from
`ISO_FORTRAN_ENV`.  The `LOCK` and `UNLOCK` statements allow one
to manipulate such a lock.  To be useful, this variable has to
be a coarray.  An example:  Let us assume we want to calculate
the factorial of the number of images in a parallel way.  One
possibility would be
```
program main
  use, intrinsic :: iso_fortran_env, only: lock_type
  implicit none
  type(lock_type), codimension[*] :: lck
  integer, codimension[*] :: i
  if (this_image() == 1) i = 1
  sync all
  lock (lck[1])
  i[1] = i[1] * this_image()
  unlock (lck[1])
  if (this_image() == 1) print *,i
end program main
```
For four images, this will dutifully print `24`.

# Collective subroutines

Data transfer between images can be repetetive to write.  For
example, setting a value on all images would require an
explicit DO loop over all images, plus explicit synchronization.

To facilitate this, the Fortran 2018 standard introduced the collective
subroutines.  Using these subroutines, you can transfer data between
images using normal (i.e. non-coarray) variables.

## Setting a value on all images - `CO_BROADCAST`

You use the subroutine `CO_BROADCAST` to set the value of variables
on all images from one particular image.  This variable can be an
array or a scalar. Here is an example:
```
program main
  integer, dimension(3) :: a
  if (this_image () == 1) then
    a = [2,3,5]
  end if
  call co_broadcast (a, 1)
  write (*,*) 'Image', this_image(), "a =", a
end program main
```
The call to co_broadcast works as if the value of `a` is
been assigned to the value of `a` on image 1.
`a` is *not* a coarray (no square brackets), and no explicit
synchronization is needed. The compiler does that for you. The
example output is
```
 Image           2 a =            2           3           5
 Image           4 a =            2           3           5
 Image           3 a =            2           3           5
 Image           1 a =            2           3           5
```

## Common reductions - sum, maximum, minimum

You often want to know the sum, maximum, minimum or product of
something that is calculated on each image. This is common
enough so that three is a subroutine for each of these tasks:
`CO_SUM`, `CO_MAX`, `CO_MIN`, respectively.  You can apply these
subroutines to scalars or arrays.

These subroutines take as argument the variable to be reduced, plus
an optional argument `RESULT_IMAGE` where the result should be
stored.  If you supply that image number, then the result is only
stored on the corresponding image, and the variables on all other
variables become undefined. If you do not supply `RESULT_IMAGE`, the
result is stored on every variable.  Here is an example without using
`RESULT_IMAGE`:
```
program main
  integer :: a
  a = this_image()
  call co_sum(a)
  write (*,*) this_image(), a
end
```
with the output
```
           2          10
           4          10
           3          10
           1          10
```
And here is a variant which used `RESULT_IMAGE` to assign
the value to image 1 only:
```
program main
  implicit none
  integer :: me, n
  me = this_image ()
  n = num_images()
  call co_sum (me, result_image = 1)
  if (this_image() == 1) then
       write (*,'(*(A,I0))') "Number of images: ", n, " sum: ", me, &
           " expected: ", n*(n+1)/2
  end if
end program main
```
with the output
```
Number of images: 4 sum: 10 expected: 10
```
Here is another example which calculates the sum, minimum and maximum
of a value which is calculated for each image. The program prints out
the values for each image, then the minimum, maximum and sum of
each element.
```
program main
  implicit none
  integer, parameter :: n = 3
  integer :: i
  real, dimension(n) :: val
  real, dimension(n) :: val_min, val_max, val_sum
  val = [(cos(0.2*i*this_image()),i=1,n)]
  write (*,'(I4," ",3F12.5)') this_image(), val
  val_min = val
  call co_min (val_min, result_image = 1)
  val_max = val
  call co_max (val_max, result_image = 1)
  val_sum = val
  call co_sum (val_sum, result_image = 1)
  if (this_image() == 1) then
     write (*,'(A,3F12.5)') "Min: ", val_min, "Max: ", val_max, &
          "Sum: ", val_sum
  end if
end program main
```
The output is, for four images
```
   4      0.69671    -0.02920    -0.73739
   2      0.92106     0.69671     0.36236
   1      0.98007     0.92106     0.82534
   3      0.82534     0.36236    -0.22720
Min:      0.69671    -0.02920    -0.73739
Max:      0.98007     0.92106     0.82534
Sum:      3.42317     1.95093     0.22310
```
## Generalized reduction - `CO_REDUCE`
There is a possibility that the reduction that is needed is not among
the supported ones above. In that case, you can define your own
function to do the reduction and call `CO_REDUCE`.

The function needs to be `PURE`, and it needs to apply the operation
to its two arguments.  It also needs to be commutative, so
`f(a,b)` needs to do the same thing as `f(b,a)`. The following
example checks if all elements of the logical variable `flag` are
true, the same way that the `ALL` intrinsic would do for normal
Fortran variables.
```
program main
  implicit none
  integer, parameter :: n = 3
  integer :: i
  logical, dimension(n) :: flag
  flag = [(cos(0.2*i*this_image()) > 0.,i=1,n)]
  write (*,'(I4," ",3L2)') this_image(), flag
  call co_reduce (flag, both, result_image=1)
  if (this_image() == 1) then
     write (*,'(A5,3L2)') "All: ", flag
  end if
contains
  pure function both (lhs,rhs) result(res)
    logical, intent(in) :: lhs,rhs
    logical :: res
    res = lhs .AND. rhs
  END FUNCTION both
end program main
```
And here is its output:
```
   2  T T T
   3  T T F
   4  T F F
   1  T T T
All:  T F F
```
# Errors, error discovery and program termination

What happens when errors occur and images terminate needs to be
defined carefully. Fortran has facilities to detect failure on
individual compute nodes and offers possibilities to deal with them.

## Image states

There are three states that an image can be in: It can be an
- *active image* if it is running normally
- *stopped image* if it has been terminated normally by reaching
the end of the main program or by executing a `STOP` statement.
- *failed image* when an image stopped working for some reason
  (for example a hardware failure) or execution of a `FAIL IMAGE`
  statement.

Once an image is in a stopped or failed state, there is no coming
back - it will always remain in that state.  An image can also be
terminated by an *error condition*; all other images should then also
be terminated by the system as soon as possible.  This is what
usually happens when you try to allocate an already allocated
variable, open a non-existent file for reading without specifying
a `STAT` variable.

## Look at the state you are in

If you synchronize with a failed or stopped image, try to
allocate or deallocate a variable there or other similar things,
what is the system to do?  Without direction from the programmer,
it will simply terminate the program (an error condition, as above).
This is not very useful as a fail-safe tactic.

However, the programmer can specify a `STAT` and optionally the
`ERRMSG` arguments to catch the error and act accordingly. It
is then possible to compare the value returned for the `STAT`
argument against predefined values from `iso_fortran_env` and
then use the intrinsic functions  `FAILED_IMAGES()` and
`STOPPED_IMAGES()` too look up which ones failed.

```
program main
  use iso_fortran_env, only : STAT_FAILED_IMAGE,  STAT_STOPPED_IMAGE
  integer :: sync_stat, alloc_stat
  sync all (stat=sync_stat)
  if (stat /= 0) then
    if (stat == STAT_FAILED_IMAGE) then
      print *,"Failed images: ", failed_images()
    else if (stat == STAT_STOPPED_IMAGE) then
      print *,"Stopped images: ", stopped_images()
    else
      print *,"Unforseen error, aborting"
      error stop
    end if
  end if
```

# Getting it to work

## Using gfortran

The [GNU Fortran compiler](https://gcc.gnu.org/onlinedocs/gfortran/)
supports [OpenCoarrays](http://www.opencoarrays.org/).  If you do not
have it in your Linux distribution, you can follow the [installation
instructions](https://github.com/sourceryinstitute/OpenCoarrays/blob/main/INSTALL.md) .
Compilation then will be done via
```
$ mpif90 hello.f90 -lcaf_mpi
```
and the program can then be run by
```
$ mpiexec -n 10 ./a.out
```

Another possibilility currently under development is the [shared
memory coarray branch](https://gcc.gnu.org/git/?p=gcc.git;a=tree;h=refs/heads/devel/coarray_native;hb=refs/heads/devel/coarray_native).
This will work without any additional libraries and currently under active development, but does not yet have all
features implemented.

## Using ifort

If you use `ifort`, you can use the `-coarray` option, as in
```
$ ifort -coarray hello.f90
```
and then run the executable. This will give you the shared memory
version.  For more details refer to the manpage of ifort.

## Using NAG Fortran

If you use `nagfor`, you can use the `-coarray` option, as in
```
$ nagfor -coarray hello.f90
```
and then run the executable. This will give yo the shared memory
version.  For more details refert to the manpage of nagfor.


