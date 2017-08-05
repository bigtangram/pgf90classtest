module m_obsNode
   implicit none
   private
   public obsNode

   type,abstract::obsNode
     integer :: id

     contains
       procedure(intrfc_mytype_ ),nopass,deferred:: mytype     ! return my type
   end type obsNode

   abstract interface
      function intrfc_mytype_()
         import obsNode
         implicit none
         character(len=:),allocatable:: intrfc_mytype_
      end function intrfc_mytype_
   end interface

contains

end module m_obsNode

module m_psNode
   use m_obsNode, only: obsNode
   implicit none
   private
   public psNode
   public obsLList

   type,extends(obsNode):: psNode
      real :: eID
      contains
          procedure,nopass::  mytype
   end type psNode

   type obsLList
     class(obsNode),allocatable:: mold
   end type obsLList

contains

function mytype()
  implicit none
  character(len=:),allocatable:: mytype
  mytype='[psNode]'
end function

end module m_psNode


program classTest
   use m_obsNode, only : obsNode
   use m_psNode, only : psNode, obsLList
   implicit none
   type(psNode), target, save ::  ps1
   class(obsNode), pointer :: ps2
   type(obsLList) :: myllist
   ps1%id=1
   ps1%eid=1.2
   ps2=>ps1
   print *, 'id,eid,type=', ps1%id, ps1%eid, trim(ps1%mytype())
   !print *, 'id,eid,type=', ps2%id, ps2%eid, trim(ps2%mytype())
   print *, 'id,eid,type=', ps2%id,          trim(ps2%mytype())
   allocate(myllist%mold,mold=ps2)
   !allocate(myllist%mold,source=ps2)
   !allocate(myllist%mold,mold=ps1)
   !allocate(myllist%mold,source=ps1)
   print *, 'id,type=', myllist%mold%id , trim(myllist%mold%mytype())
   deallocate(myllist%mold)
   print *, 'id,eid,type=', ps1%id, ps1%eid, trim(ps1%mytype())
   !print *, 'id,eid,type=', ps2%id, ps2%eid, trim(ps2%mytype())
   print *, 'id,eid,type=', ps2%id,          trim(ps2%mytype())

end program classTest
