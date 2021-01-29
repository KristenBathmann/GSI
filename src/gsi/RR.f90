module RR
use kinds, only: r_kind,i_kind,r_double
!use radinfo, only: nchan1,nchan2,nchan3,nvecs1,nvecs2,nvecs3, &
!                   Evecs1,Evecs2,Evecs3,noise1,noise2,noise3, &
!                   rmean1,rmean2,rmean3
!use radinfo, only: indR, nch_iasia
implicit none
public:: read_eig,reconstruct
public:: Evecs1,Evecs2,Evecs3,noise1,noise2,noise3
public:: rmean1,rmean2,rmean3
public:: nchan1,nchan2,nchan3,nvecs1,nvecs2,nvecs3
public:: indR,nch_iasia,wav
real(r_double),allocatable,dimension(:,:)::Evecs1,Evecs2,Evecs3
real(r_double),allocatable,dimension(:):: noise1,noise2,noise3,rmean1,rmean2,rmean3,wav
integer(i_kind):: nchan1,nchan2,nchan3,nvecs1,nvecs2,nvecs3,nvecs,nch_iasia
integer(i_kind),dimension(:),allocatable:: indR
contains
subroutine read_eig(fname,nchan,nvecs,first_chan,Evecs,noise,rad_mean)
use HDF5
use iso_c_binding
implicit none
character(len=*),intent(in):: fname
character(len=12)::evecstr='Eigenvectors'
character(4):: noisestr='Nedr'
character(4)::radmstr="Mean"
character(len=12)::fcs='FirstChannel'
character(len=11)::ncs='NbrChannels'
character(len=15)::nvs='NbrEigenvectors'
integer(i_kind),intent(inout):: nchan, nvecs,first_chan 
real(r_double),dimension(:,:),allocatable,intent(inout):: Evecs 
real(r_double),dimension(:),allocatable,intent(inout):: noise,rad_mean
integer(HID_T):: lu,eveci,evali,noisei,radmi,fci,nci,nvi
integer(i_kind):: stat
type(C_PTR):: fc_ptr,nc_ptr,nv_ptr,evec_ptr,noise_ptr,mean_ptr
call h5open_f(stat)
call h5fopen_f(trim(fname),H5F_ACC_RDONLY_F,lu,stat)
call h5dopen_f(lu,evecstr,eveci,stat)
call h5dopen_f(lu,noisestr,noisei,stat)
call h5dopen_f(lu,radmstr,radmi,stat)
call h5aopen_f(lu,fcs,fci,stat)
call h5aopen_f(lu,ncs,nci,stat)
call h5aopen_f(lu,nvs,nvi,stat)
!need to read the attributes
fc_ptr=C_LOC(first_chan)
nc_ptr=C_LOC(nchan)
nv_ptr=C_LOC(nvecs)
call h5aread_f(fci,H5T_NATIVE_INTEGER,fc_ptr,stat)
call h5aread_f(nci,H5T_NATIVE_INTEGER,nc_ptr,stat)
call h5aread_f(nvi,H5T_NATIVE_INTEGER,nv_ptr,stat)
allocate(Evecs(nchan,nvecs))
allocate(noise(nchan),rad_mean(nchan))
evec_ptr=C_LOC(Evecs)
noise_ptr=C_LOC(noise)
mean_ptr=C_LOC(rad_mean)
call H5Dread_f(eveci,H5T_NATIVE_DOUBLE,evec_ptr,stat)
call H5Dread_f(noisei,H5T_NATIVE_DOUBLE,noise_ptr,stat)
call H5Dread_f(radmi,H5T_NATIVE_DOUBLE,mean_ptr,stat)
!close everything
call h5aclose_f(fci,stat)
call h5aclose_f(nci,stat)
call h5aclose_f(nvi,stat)
call h5dclose_f(eveci,stat)
end subroutine read_eig

subroutine reconstruct(PC1,PC2,PC3,sq,Rad)
!use radinfo, only: indR, nch_iasia
implicit none
real(r_double),dimension(:,:),intent(in):: PC1,PC2,PC3,sq
real(r_double),dimension(:),intent(out):: Rad
integer(i_kind)::r,rs,ds,totchan,coun
real(r_double):: Rsum1,Rsum2,Rsum3

Rsum1=0.0_r_double
Rsum2=0.0_r_double
Rsum3=0.0_r_double
Rad=0.0_r_double
totchan=nchan1+nchan2+nchan3
coun=1
do r=1,totchan
  if (coun<=nch_iasia) then
    if (r==indR(coun)) then
      if (r>(nchan1+nchan2)) then !Band 3
        rs=r-nchan1-nchan2
        Rsum3=0.0_r_double
        do ds=1,nvecs3
           Rsum3=Rsum3+PC3(1,ds)*Evecs3(rs,ds)
        enddo
        Rsum3=Rsum3*sq(3,1)
        Rad(coun)=noise3(rs)*(rmean3(rs)+Rsum3)
      elseif (r>(nchan1)) then !Band 2
        rs=r-nchan1
        Rsum2=0.0_r_double
        do ds=1,nvecs2
           Rsum2=Rsum2+PC2(1,ds)*Evecs2(rs,ds)
        enddo
        Rsum2=Rsum2*sq(2,1)
        Rad(coun)=noise2(rs)*(rmean2(rs)+Rsum2)
      else !Band 1
        rs=r
        Rsum1=0.0_r_double
        do ds=1,nvecs1
         Rsum1=Rsum1+PC1(1,ds)*Evecs1(rs,ds)
        enddo
        Rsum1=Rsum1*sq(1,1)
        Rad(coun)=noise1(rs)*(rmean1(rs)+Rsum1)
      endif
      coun=coun+1
    endif
  endif
enddo

end subroutine reconstruct

subroutine inv_planck(Rad,wave,BT)
implicit none
real(r_double),intent(in):: Rad
real(r_double),intent(in):: wave
real(r_double),intent(out):: BT
real,parameter:: n1=1.191109E-16
real,parameter:: n2=0.01438432
integer::dc,r
real(8):: ln

BT=0.0_r_double
ln=n1*(wave**3)
ln=(ln/Rad)+1
BT=n2*wave/log(ln)
end subroutine inv_planck
end module RR
