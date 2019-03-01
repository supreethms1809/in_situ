












! Copyright (c) 2013,  Los Alamos National Security, LLC (LANS)
! and the University Corporation for Atmospheric Research (UCAR).
!
! Unless noted otherwise source code is licensed under the BSD license.
! Additional copyright and license information can be found in the LICENSE file
! distributed with this code, or at http://mpas-dev.github.com/license.html
!
module mpas_io_output

   use mpas_grid_types
   use mpas_dmpar
   use mpas_sort
   use mpas_configure
   use mpas_io_streams

   integer, parameter :: OUTPUT = 1
   integer, parameter :: RESTART = 2
   integer, parameter :: SFC = 3
 
   type io_output_object
      character (len=StrKIND) :: filename
      integer :: stream

      integer :: time

      type (MPAS_Stream_type) :: io_stream
   end type io_output_object

   private :: mpas_insert_string_suffix

   contains

   subroutine mpas_output_state_init(output_obj, domain, stream, outputSuffix)!{{{

      implicit none

      type (io_output_object), intent(inout) :: output_obj
      type (domain_type), intent(in) :: domain
      character (len=*) :: stream
      character (len=*), optional :: outputSuffix

      character (len=StrKIND) :: tempfilename

      type (block_type), pointer :: block_ptr

      block_ptr => domain % blocklist

      if (trim(stream) == 'OUTPUT') then
         if(present(outputSuffix)) then
            call mpas_insert_string_suffix(config_output_name, outputSuffix, tempfilename)
         else
            tempfilename = config_output_name
         end if
         output_obj % filename = trim(tempfilename)
         output_obj % stream = OUTPUT
      else if (trim(stream) == 'RESTART') then
         if(present(outputSuffix)) then
            call mpas_insert_string_suffix(config_restart_name, outputSuffix, tempfilename)
            open(22,file=trim(config_restart_timestamp_name),form='formatted',status='replace')
            write(22,*) outputSuffix
            close(22)
         else
            tempfilename = config_restart_name
         end if
         output_obj % filename = trim(tempfilename)
         output_obj % stream = RESTART
      else if (trim(stream) == 'SFC') then
         ! Keep filename as whatever was set by the user
         output_obj % stream = SFC
      end if

      ! For now, we assume that a domain consists only of one block,
      !   although in future, work needs to be done to write model state
      !   from many distributed blocks
      call mpas_io_output_init(domain, output_obj, domain % dminfo, &
                          block_ptr % mesh &
                         )

   end subroutine mpas_output_state_init!}}}

   subroutine mpas_insert_string_suffix(stream, suffix, filename)!{{{

      implicit none

      character (len=*), intent(in) :: stream
      character (len=*), intent(in) :: suffix
      character (len=*), intent(out) :: filename
      integer :: length, i

      filename = trim(stream) // '.' // trim(suffix)

      length = len_trim(stream)
      do i=length-1,1,-1
         if(stream(i:i) == '.') then
            filename = trim(stream(:i)) // trim(suffix) // trim(stream(i:))
            exit
         end if
      end do

      do i=1,len_trim(filename)
         if (filename(i:i) == ':') filename(i:i) = '.'
      end do

   end subroutine mpas_insert_string_suffix!}}}

   subroutine mpas_output_state_for_domain(output_obj, domain, itime)!{{{
   
      implicit none
   
      type (io_output_object), intent(inout) :: output_obj
      type (domain_type), intent(inout) :: domain
      integer, intent(in) :: itime

      type(block_type), pointer :: block_ptr

      integer :: nCells, nEdges, nVertices, vertexDegree
      integer :: maxEdges, maxEdges2, nEdgesSolve, nCellsSolve, nVerticesSolve
      integer :: ierr
      integer :: i, j
      type (field2dInteger), pointer :: cellsOnCell_save, edgesOnCell_save, verticesOnCell_save, &
                               cellsOnEdge_save, verticesOnEdge_save, edgesOnEdge_save, &
                               cellsOnVertex_save, edgesOnVertex_save

      type (field2dInteger), pointer :: cellsOnCell_ptr, edgesOnCell_ptr, verticesOnCell_ptr, &
                               cellsOnEdge_ptr, verticesOnEdge_ptr, edgesOnEdge_ptr, &
                               cellsOnVertex_ptr, edgesOnVertex_ptr

      output_obj % time = itime

      !
      ! Convert connectivity information from local to global indices
      ! Needs to be done block by block
      !
      ! Also, backup local indices to be copied back into blocks after output is complete.
      !
      allocate(cellsOnCell_save)
      allocate(edgesOnCell_save) 
      allocate(verticesOnCell_save)
      allocate(cellsOnEdge_save)
      allocate(verticesOnEdge_save)
      allocate(edgesOnEdge_save)
      allocate(cellsOnVertex_save)
      allocate(edgesOnVertex_save)

      cellsOnCell_ptr => cellsOnCell_save
      edgesOnCell_ptr => edgesOnCell_save 
      verticesOnCell_ptr => verticesOnCell_save
      cellsOnEdge_ptr => cellsOnEdge_save 
      verticesOnEdge_ptr => verticesOnEdge_save 
      edgesOnEdge_ptr => edgesOnEdge_save
      cellsOnVertex_ptr => cellsOnVertex_save 
      edgesOnVertex_ptr => edgesOnVertex_save

      block_ptr => domain % blocklist
      do while(associated(block_ptr))
        maxEdges = block_ptr % mesh % maxEdges
        maxEdges2 = block_ptr % mesh % maxEdges2
        vertexDegree = block_ptr % mesh % vertexDegree
        nCells = block_ptr % mesh % nCells
        nEdges = block_ptr % mesh % nEdges
        nVertices = block_ptr % mesh % nVertices
        nCellsSolve = block_ptr % mesh % nCellsSolve
        nEdgesSolve = block_ptr % mesh % nEdgesSolve
        nVerticesSolve = block_ptr % mesh % nVerticesSolve

        nullify(cellsOncell_ptr % ioinfo)
        cellsOncell_ptr % array => block_ptr % mesh % cellsOncell % array
        allocate(block_ptr % mesh % cellsOnCell % array(maxEdges, nCells+1))

        nullify(edgesOnCell_ptr % ioinfo)
        edgesOnCell_ptr % array => block_ptr % mesh % edgesOnCell % array
        allocate(block_ptr % mesh % edgesOnCell % array(maxEdges, nCells+1))

        nullify(verticesOnCell_ptr % ioinfo)
        verticesOnCell_ptr % array => block_ptr % mesh % verticesOnCell % array
        allocate(block_ptr % mesh % verticesOnCell % array(maxEdges, nCells+1))

        nullify(cellsOnEdge_ptr % ioinfo)
        cellsOnEdge_ptr % array => block_ptr % mesh % cellsOnEdge % array
        allocate(block_ptr % mesh % cellsOnEdge % array(2, nEdges+1))

        nullify(verticesOnEdge_ptr % ioinfo)
        verticesOnEdge_ptr % array => block_ptr % mesh % verticesOnEdge % array
        allocate(block_ptr % mesh % verticesOnEdge % array(2, nEdges+1))

        nullify(edgesOnEdge_ptr % ioinfo)
        edgesOnEdge_ptr % array => block_ptr % mesh % edgesOnEdge % array
        allocate(block_ptr % mesh % edgesOnEdge % array(maxEdges2, nEdges+1))

        nullify(cellsOnVertex_ptr % ioinfo)
        cellsOnVertex_ptr % array => block_ptr % mesh % cellsOnVertex % array
        allocate(block_ptr % mesh % cellsOnVertex % array(vertexDegree, nVertices+1))

        nullify(edgesOnVertex_ptr % ioinfo)
        edgesOnVertex_ptr % array => block_ptr % mesh % edgesOnVertex % array
        allocate(block_ptr % mesh % edgesOnVertex % array(vertexDegree, nVertices+1))

        do i = 1, nCellsSolve
          do j = 1, block_ptr % mesh % nEdgesOnCell % array(i)
            block_ptr % mesh % cellsOnCell % array(j, i) = block_ptr % mesh % indexToCellID % array(cellsOnCell_ptr % array(j, i))
            block_ptr % mesh % edgesOnCell % array(j, i) = block_ptr % mesh % indexToEdgeID % array(edgesOnCell_ptr % array(j, i))
            block_ptr % mesh % verticesOnCell % array(j, i) = block_ptr % mesh % indexToVertexID % array(verticesOnCell_ptr % array(j, i))
          end do

          block_ptr % mesh % cellsOnCell % array(block_ptr % mesh % nEdgesOnCell % array(i) + 1:maxEdges, i) = nCells+1
          block_ptr % mesh % edgesOnCell % array(block_ptr % mesh % nEdgesOnCell % array(i) + 1:maxEdges, i) = nEdges+1
          block_ptr % mesh % verticesOnCell % array(block_ptr % mesh % nEdgesOnCell % array(i) + 1:maxEdges, i) = nVertices+1
        end do

        do i = 1, nEdgesSolve
          block_ptr % mesh % cellsOnEdge % array(1, i) = block_ptr % mesh % indexToCellID % array(cellsOnEdge_ptr % array(1, i))
          block_ptr % mesh % cellsOnEdge % array(2, i) = block_ptr % mesh % indexToCellID % array(cellsOnEdge_ptr % array(2, i))

          block_ptr % mesh % verticesOnedge % array(1, i) = block_ptr % mesh % indexToVertexID % array(verticesOnEdge_ptr % array(1,i))
          block_ptr % mesh % verticesOnedge % array(2, i) = block_ptr % mesh % indexToVertexID % array(verticesOnEdge_ptr % array(2,i))

          do j = 1, block_ptr % mesh % nEdgesOnEdge % array(i)
            block_ptr % mesh % edgesOnEdge % array(j, i) = block_ptr % mesh % indexToEdgeID % array(edgesOnEdge_ptr % array(j, i))
          end do

          block_ptr % mesh % edgesOnEdge % array(block_ptr % mesh % nEdgesOnEdge % array(i)+1:maxEdges2, i) = nEdges+1
        end do

        do i = 1, nVerticesSolve
          do j = 1, vertexDegree
            block_ptr % mesh % cellsOnVertex % array(j, i) = block_ptr % mesh % indexToCellID % array(cellsOnVertex_ptr % array(j, i))
            block_ptr % mesh % edgesOnVertex % array(j, i) = block_ptr % mesh % indexToEdgeID % array(edgesOnVertex_ptr % array(j, i))
          end do
        end do

        block_ptr => block_ptr % next
        if(associated(block_ptr)) then
          allocate(cellsOnCell_ptr % next)
          allocate(edgesOnCell_ptr % next)
          allocate(verticesOnCell_ptr % next)
          allocate(cellsOnEdge_ptr % next)
          allocate(verticesOnEdge_ptr % next)
          allocate(edgesOnEdge_ptr % next)
          allocate(cellsOnVertex_ptr % next)
          allocate(edgesOnVertex_ptr % next)

          cellsOnCell_ptr => cellsOnCell_ptr % next
          edgesOnCell_ptr => edgesOnCell_ptr % next
          verticesOnCell_ptr => verticesOnCell_ptr % next
          cellsOnEdge_ptr => cellsOnEdge_ptr % next
          verticesOnEdge_ptr => verticesOnEdge_ptr % next
          edgesOnEdge_ptr => edgesOnEdge_ptr % next
          cellsOnVertex_ptr => cellsOnVertex_ptr % next
          edgesOnVertex_ptr => edgesOnVertex_ptr % next
        end if

        nullify(cellsOnCell_ptr % next)
        nullify(edgesOnCell_ptr % next)
        nullify(verticesOnCell_ptr % next)
        nullify(cellsOnEdge_ptr % next)
        nullify(verticesOnEdge_ptr % next)
        nullify(edgesOnEdge_ptr % next)
        nullify(cellsOnVertex_ptr % next)
        nullify(edgesOnVertex_ptr % next)
      end do

      ! Write output file
      call MPAS_writeStream(output_obj % io_stream, output_obj % time, ierr)

      ! Converge indices back to local indices, and deallocate all temporary arrays.
      cellsOnCell_ptr => cellsOnCell_save
      edgesOnCell_ptr => edgesOnCell_save 
      verticesOnCell_ptr => verticesOnCell_save
      cellsOnEdge_ptr => cellsOnEdge_save 
      verticesOnEdge_ptr => verticesOnEdge_save 
      edgesOnEdge_ptr => edgesOnEdge_save
      cellsOnVertex_ptr => cellsOnVertex_save 
      edgesOnVertex_ptr => edgesOnVertex_save

      block_ptr => domain % blocklist
      do while(associated(block_ptr))

        deallocate(block_ptr % mesh % cellsOnCell % array)
        deallocate(block_ptr % mesh % edgesOnCell % array)
        deallocate(block_ptr % mesh % verticesOnCell % array)
        deallocate(block_ptr % mesh % cellsOnEdge % array)
        deallocate(block_ptr % mesh % verticesOnEdge % array)
        deallocate(block_ptr % mesh % edgesOnEdge % array)
        deallocate(block_ptr % mesh % cellsOnVertex % array)
        deallocate(block_ptr % mesh % edgesOnVertex % array)

        block_ptr % mesh % cellsOncell % array => cellsOnCell_ptr % array
        block_ptr % mesh % edgesOnCell % array => edgesOnCell_ptr % array
        block_ptr % mesh % verticesOnCell % array => verticesOnCell_ptr % array
        block_ptr % mesh % cellsOnEdge % array => cellsOnEdge_ptr % array
        block_ptr % mesh % verticesOnEdge % array => verticesOnEdge_ptr % array
        block_ptr % mesh % edgesOnEdge % array => edgesOnEdge_ptr % array
        block_ptr % mesh % cellsOnVertex % array => cellsOnVertex_ptr % array
        block_ptr % mesh % edgesOnVertex % array => edgesOnVertex_ptr % array

        nullify(cellsOnCell_ptr % array)
        nullify(edgesOnCell_ptr % array)
        nullify(verticesOnCell_ptr % array)
        nullify(cellsOnEdge_ptr % array)
        nullify(verticesOnEdge_ptr % array)
        nullify(edgesOnEdge_ptr % array)
        nullify(cellsOnVertex_ptr % array)
        nullify(edgesOnVertex_ptr % array)

        block_ptr => block_ptr % next
        cellsOnCell_ptr => cellsOnCell_ptr % next
        edgesOnCell_ptr => edgesOnCell_ptr % next
        verticesOnCell_ptr => verticesOnCell_ptr % next
        cellsOnEdge_ptr => cellsOnEdge_ptr % next
        verticesOnEdge_ptr => verticesOnEdge_ptr % next
        edgesOnEdge_ptr => edgesOnEdge_ptr % next
        cellsOnVertex_ptr => cellsOnVertex_ptr % next
        edgesOnVertex_ptr => edgesOnVertex_ptr % next
      end do

      call mpas_deallocate_field(cellsOnCell_save)
      call mpas_deallocate_field(edgesOnCell_save) 
      call mpas_deallocate_field(verticesOnCell_save)
      call mpas_deallocate_field(cellsOnEdge_save)
      call mpas_deallocate_field(verticesOnEdge_save)
      call mpas_deallocate_field(edgesOnEdge_save)
      call mpas_deallocate_field(cellsOnVertex_save)
      call mpas_deallocate_field(edgesOnVertex_save)



   end subroutine mpas_output_state_for_domain!}}}

   subroutine mpas_output_state_finalize(output_obj, dminfo)!{{{

      implicit none

      type (io_output_object), intent(inout) :: output_obj
      type (dm_info), intent(in) :: dminfo

      call mpas_io_output_finalize(output_obj, dminfo)

   end subroutine mpas_output_state_finalize!}}}

   subroutine mpas_io_output_init( domain, output_obj, &!{{{
                              dminfo, &
                              mesh &
                            )
 
      implicit none
 
      type (domain_type), intent(in) :: domain
      type (io_output_object), intent(inout) :: output_obj
      type (dm_info), intent(in) :: dminfo
      type (mesh_type), intent(in) :: mesh
 
      integer :: nferr, ierr
      integer, dimension(10) :: dimlist
      character (len=StrKIND*4) :: runCmd

      if(len(trim(domain % history)) > 0) then
          write(runCmd,'(a,a,i0,a,a,a)') trim(domain % history),' mpirun -n ',domain % dminfo % nProcs, ' ', trim(domain % coreName), '_model; '
      else
          write(runCmd,'(a,i0,a,a,a)') 'mpirun -n ',domain % dminfo % nProcs, ' ', trim(domain % coreName), '_model; '
      end if
 
      call MPAS_createStream(output_obj % io_stream, trim(output_obj % filename), MPAS_IO_PNETCDF, MPAS_IO_WRITE, 1, nferr)

      if ((domain % blocklist % state % time_levs(1) % state % tracers % ioinfo % output .and. output_obj % stream == OUTPUT) &
.or. &
          (domain % blocklist % state % time_levs(1) % state % tracers % ioinfo % restart .and. output_obj % stream == RESTART) &
.or. &
          (domain % blocklist % state % time_levs(1) % state % tracers % ioinfo % sfc .and. output_obj % stream == SFC)) then
         call MPAS_streamAddField(output_obj % io_stream, domain % blocklist % state % time_levs(1) % state % tracers, ierr)
      end if

      if ((domain % blocklist % state % time_levs(1) % state % normalVelocity % ioinfo % output .and. output_obj % stream == &
OUTPUT) .or. &
          (domain % blocklist % state % time_levs(1) % state % normalVelocity % ioinfo % restart .and. output_obj % stream == &
RESTART) .or. &
          (domain % blocklist % state % time_levs(1) % state % normalVelocity % ioinfo % sfc .and. output_obj % stream == SFC)) &
then
         call MPAS_streamAddField(output_obj % io_stream, domain % blocklist % state % time_levs(1) % state % normalVelocity, ierr)
      end if

      if ((domain % blocklist % state % time_levs(1) % state % layerThickness % ioinfo % output .and. output_obj % stream == &
OUTPUT) .or. &
          (domain % blocklist % state % time_levs(1) % state % layerThickness % ioinfo % restart .and. output_obj % stream == &
RESTART) .or. &
          (domain % blocklist % state % time_levs(1) % state % layerThickness % ioinfo % sfc .and. output_obj % stream == SFC)) &
then
         call MPAS_streamAddField(output_obj % io_stream, domain % blocklist % state % time_levs(1) % state % layerThickness, ierr)
      end if

      if ((domain % blocklist % state % time_levs(1) % state % ssh % ioinfo % output .and. output_obj % stream == OUTPUT) .or. &
          (domain % blocklist % state % time_levs(1) % state % ssh % ioinfo % restart .and. output_obj % stream == RESTART) .or. &
          (domain % blocklist % state % time_levs(1) % state % ssh % ioinfo % sfc .and. output_obj % stream == SFC)) then
         call MPAS_streamAddField(output_obj % io_stream, domain % blocklist % state % time_levs(1) % state % ssh, ierr)
      end if

      if ((domain % blocklist % state % time_levs(1) % state % highFreqThickness % ioinfo % output .and. output_obj % stream == &
OUTPUT) .or. &
          (domain % blocklist % state % time_levs(1) % state % highFreqThickness % ioinfo % restart .and. output_obj % stream == &
RESTART) .or. &
          (domain % blocklist % state % time_levs(1) % state % highFreqThickness % ioinfo % sfc .and. output_obj % stream == SFC)) &
then
         call MPAS_streamAddField(output_obj % io_stream, domain % blocklist % state % time_levs(1) % state % highFreqThickness, &
ierr)
      end if

      if ((domain % blocklist % state % time_levs(1) % state % lowFreqDivergence % ioinfo % output .and. output_obj % stream == &
OUTPUT) .or. &
          (domain % blocklist % state % time_levs(1) % state % lowFreqDivergence % ioinfo % restart .and. output_obj % stream == &
RESTART) .or. &
          (domain % blocklist % state % time_levs(1) % state % lowFreqDivergence % ioinfo % sfc .and. output_obj % stream == SFC)) &
then
         call MPAS_streamAddField(output_obj % io_stream, domain % blocklist % state % time_levs(1) % state % lowFreqDivergence, &
ierr)
      end if

      if ((domain % blocklist % state % time_levs(1) % state % normalBarotropicVelocity % ioinfo % output .and. output_obj % &
stream == OUTPUT) .or. &
          (domain % blocklist % state % time_levs(1) % state % normalBarotropicVelocity % ioinfo % restart .and. output_obj % &
stream == RESTART) .or. &
          (domain % blocklist % state % time_levs(1) % state % normalBarotropicVelocity % ioinfo % sfc .and. output_obj % stream &
== SFC)) then
         call MPAS_streamAddField(output_obj % io_stream, domain % blocklist % state % time_levs(1) % state % &
normalBarotropicVelocity, ierr)
      end if

      if ((domain % blocklist % state % time_levs(1) % state % normalBarotropicVelocitySubcycle % ioinfo % output .and. output_obj &
% stream == OUTPUT) .or. &
          (domain % blocklist % state % time_levs(1) % state % normalBarotropicVelocitySubcycle % ioinfo % restart .and. &
output_obj % stream == RESTART) .or. &
          (domain % blocklist % state % time_levs(1) % state % normalBarotropicVelocitySubcycle % ioinfo % sfc .and. output_obj % &
stream == SFC)) then
         call MPAS_streamAddField(output_obj % io_stream, domain % blocklist % state % time_levs(1) % state % &
normalBarotropicVelocitySubcycle, ierr)
      end if

      if ((domain % blocklist % state % time_levs(1) % state % sshSubcycle % ioinfo % output .and. output_obj % stream == OUTPUT) &
.or. &
          (domain % blocklist % state % time_levs(1) % state % sshSubcycle % ioinfo % restart .and. output_obj % stream == &
RESTART) .or. &
          (domain % blocklist % state % time_levs(1) % state % sshSubcycle % ioinfo % sfc .and. output_obj % stream == SFC)) then
         call MPAS_streamAddField(output_obj % io_stream, domain % blocklist % state % time_levs(1) % state % sshSubcycle, ierr)
      end if

      if ((domain % blocklist % state % time_levs(1) % state % normalBaroclinicVelocity % ioinfo % output .and. output_obj % &
stream == OUTPUT) .or. &
          (domain % blocklist % state % time_levs(1) % state % normalBaroclinicVelocity % ioinfo % restart .and. output_obj % &
stream == RESTART) .or. &
          (domain % blocklist % state % time_levs(1) % state % normalBaroclinicVelocity % ioinfo % sfc .and. output_obj % stream &
== SFC)) then
         call MPAS_streamAddField(output_obj % io_stream, domain % blocklist % state % time_levs(1) % state % &
normalBaroclinicVelocity, ierr)
      end if

      if ((domain % blocklist % mesh % latCell % ioinfo % output .and. output_obj % stream == OUTPUT) .or. &
          (domain % blocklist % mesh % latCell % ioinfo % restart .and. output_obj % stream == RESTART) .or. &
          (domain % blocklist % mesh % latCell % ioinfo % sfc .and. output_obj % stream == SFC)) then
         call MPAS_streamAddField(output_obj % io_stream, domain % blocklist % mesh % latCell, ierr)
      end if

      if ((domain % blocklist % mesh % lonCell % ioinfo % output .and. output_obj % stream == OUTPUT) .or. &
          (domain % blocklist % mesh % lonCell % ioinfo % restart .and. output_obj % stream == RESTART) .or. &
          (domain % blocklist % mesh % lonCell % ioinfo % sfc .and. output_obj % stream == SFC)) then
         call MPAS_streamAddField(output_obj % io_stream, domain % blocklist % mesh % lonCell, ierr)
      end if

      if ((domain % blocklist % mesh % xCell % ioinfo % output .and. output_obj % stream == OUTPUT) .or. &
          (domain % blocklist % mesh % xCell % ioinfo % restart .and. output_obj % stream == RESTART) .or. &
          (domain % blocklist % mesh % xCell % ioinfo % sfc .and. output_obj % stream == SFC)) then
         call MPAS_streamAddField(output_obj % io_stream, domain % blocklist % mesh % xCell, ierr)
      end if

      if ((domain % blocklist % mesh % yCell % ioinfo % output .and. output_obj % stream == OUTPUT) .or. &
          (domain % blocklist % mesh % yCell % ioinfo % restart .and. output_obj % stream == RESTART) .or. &
          (domain % blocklist % mesh % yCell % ioinfo % sfc .and. output_obj % stream == SFC)) then
         call MPAS_streamAddField(output_obj % io_stream, domain % blocklist % mesh % yCell, ierr)
      end if

      if ((domain % blocklist % mesh % zCell % ioinfo % output .and. output_obj % stream == OUTPUT) .or. &
          (domain % blocklist % mesh % zCell % ioinfo % restart .and. output_obj % stream == RESTART) .or. &
          (domain % blocklist % mesh % zCell % ioinfo % sfc .and. output_obj % stream == SFC)) then
         call MPAS_streamAddField(output_obj % io_stream, domain % blocklist % mesh % zCell, ierr)
      end if

      if ((domain % blocklist % mesh % indexToCellID % ioinfo % output .and. output_obj % stream == OUTPUT) .or. &
          (domain % blocklist % mesh % indexToCellID % ioinfo % restart .and. output_obj % stream == RESTART) .or. &
          (domain % blocklist % mesh % indexToCellID % ioinfo % sfc .and. output_obj % stream == SFC)) then
         call MPAS_streamAddField(output_obj % io_stream, domain % blocklist % mesh % indexToCellID, ierr)
      end if

      if ((domain % blocklist % mesh % latEdge % ioinfo % output .and. output_obj % stream == OUTPUT) .or. &
          (domain % blocklist % mesh % latEdge % ioinfo % restart .and. output_obj % stream == RESTART) .or. &
          (domain % blocklist % mesh % latEdge % ioinfo % sfc .and. output_obj % stream == SFC)) then
         call MPAS_streamAddField(output_obj % io_stream, domain % blocklist % mesh % latEdge, ierr)
      end if

      if ((domain % blocklist % mesh % lonEdge % ioinfo % output .and. output_obj % stream == OUTPUT) .or. &
          (domain % blocklist % mesh % lonEdge % ioinfo % restart .and. output_obj % stream == RESTART) .or. &
          (domain % blocklist % mesh % lonEdge % ioinfo % sfc .and. output_obj % stream == SFC)) then
         call MPAS_streamAddField(output_obj % io_stream, domain % blocklist % mesh % lonEdge, ierr)
      end if

      if ((domain % blocklist % mesh % xEdge % ioinfo % output .and. output_obj % stream == OUTPUT) .or. &
          (domain % blocklist % mesh % xEdge % ioinfo % restart .and. output_obj % stream == RESTART) .or. &
          (domain % blocklist % mesh % xEdge % ioinfo % sfc .and. output_obj % stream == SFC)) then
         call MPAS_streamAddField(output_obj % io_stream, domain % blocklist % mesh % xEdge, ierr)
      end if

      if ((domain % blocklist % mesh % yEdge % ioinfo % output .and. output_obj % stream == OUTPUT) .or. &
          (domain % blocklist % mesh % yEdge % ioinfo % restart .and. output_obj % stream == RESTART) .or. &
          (domain % blocklist % mesh % yEdge % ioinfo % sfc .and. output_obj % stream == SFC)) then
         call MPAS_streamAddField(output_obj % io_stream, domain % blocklist % mesh % yEdge, ierr)
      end if

      if ((domain % blocklist % mesh % zEdge % ioinfo % output .and. output_obj % stream == OUTPUT) .or. &
          (domain % blocklist % mesh % zEdge % ioinfo % restart .and. output_obj % stream == RESTART) .or. &
          (domain % blocklist % mesh % zEdge % ioinfo % sfc .and. output_obj % stream == SFC)) then
         call MPAS_streamAddField(output_obj % io_stream, domain % blocklist % mesh % zEdge, ierr)
      end if

      if ((domain % blocklist % mesh % indexToEdgeID % ioinfo % output .and. output_obj % stream == OUTPUT) .or. &
          (domain % blocklist % mesh % indexToEdgeID % ioinfo % restart .and. output_obj % stream == RESTART) .or. &
          (domain % blocklist % mesh % indexToEdgeID % ioinfo % sfc .and. output_obj % stream == SFC)) then
         call MPAS_streamAddField(output_obj % io_stream, domain % blocklist % mesh % indexToEdgeID, ierr)
      end if

      if ((domain % blocklist % mesh % latVertex % ioinfo % output .and. output_obj % stream == OUTPUT) .or. &
          (domain % blocklist % mesh % latVertex % ioinfo % restart .and. output_obj % stream == RESTART) .or. &
          (domain % blocklist % mesh % latVertex % ioinfo % sfc .and. output_obj % stream == SFC)) then
         call MPAS_streamAddField(output_obj % io_stream, domain % blocklist % mesh % latVertex, ierr)
      end if

      if ((domain % blocklist % mesh % lonVertex % ioinfo % output .and. output_obj % stream == OUTPUT) .or. &
          (domain % blocklist % mesh % lonVertex % ioinfo % restart .and. output_obj % stream == RESTART) .or. &
          (domain % blocklist % mesh % lonVertex % ioinfo % sfc .and. output_obj % stream == SFC)) then
         call MPAS_streamAddField(output_obj % io_stream, domain % blocklist % mesh % lonVertex, ierr)
      end if

      if ((domain % blocklist % mesh % xVertex % ioinfo % output .and. output_obj % stream == OUTPUT) .or. &
          (domain % blocklist % mesh % xVertex % ioinfo % restart .and. output_obj % stream == RESTART) .or. &
          (domain % blocklist % mesh % xVertex % ioinfo % sfc .and. output_obj % stream == SFC)) then
         call MPAS_streamAddField(output_obj % io_stream, domain % blocklist % mesh % xVertex, ierr)
      end if

      if ((domain % blocklist % mesh % yVertex % ioinfo % output .and. output_obj % stream == OUTPUT) .or. &
          (domain % blocklist % mesh % yVertex % ioinfo % restart .and. output_obj % stream == RESTART) .or. &
          (domain % blocklist % mesh % yVertex % ioinfo % sfc .and. output_obj % stream == SFC)) then
         call MPAS_streamAddField(output_obj % io_stream, domain % blocklist % mesh % yVertex, ierr)
      end if

      if ((domain % blocklist % mesh % zVertex % ioinfo % output .and. output_obj % stream == OUTPUT) .or. &
          (domain % blocklist % mesh % zVertex % ioinfo % restart .and. output_obj % stream == RESTART) .or. &
          (domain % blocklist % mesh % zVertex % ioinfo % sfc .and. output_obj % stream == SFC)) then
         call MPAS_streamAddField(output_obj % io_stream, domain % blocklist % mesh % zVertex, ierr)
      end if

      if ((domain % blocklist % mesh % indexToVertexID % ioinfo % output .and. output_obj % stream == OUTPUT) .or. &
          (domain % blocklist % mesh % indexToVertexID % ioinfo % restart .and. output_obj % stream == RESTART) .or. &
          (domain % blocklist % mesh % indexToVertexID % ioinfo % sfc .and. output_obj % stream == SFC)) then
         call MPAS_streamAddField(output_obj % io_stream, domain % blocklist % mesh % indexToVertexID, ierr)
      end if

      if ((domain % blocklist % mesh % meshDensity % ioinfo % output .and. output_obj % stream == OUTPUT) .or. &
          (domain % blocklist % mesh % meshDensity % ioinfo % restart .and. output_obj % stream == RESTART) .or. &
          (domain % blocklist % mesh % meshDensity % ioinfo % sfc .and. output_obj % stream == SFC)) then
         call MPAS_streamAddField(output_obj % io_stream, domain % blocklist % mesh % meshDensity, ierr)
      end if

      if ((domain % blocklist % mesh % meshScalingDel2 % ioinfo % output .and. output_obj % stream == OUTPUT) .or. &
          (domain % blocklist % mesh % meshScalingDel2 % ioinfo % restart .and. output_obj % stream == RESTART) .or. &
          (domain % blocklist % mesh % meshScalingDel2 % ioinfo % sfc .and. output_obj % stream == SFC)) then
         call MPAS_streamAddField(output_obj % io_stream, domain % blocklist % mesh % meshScalingDel2, ierr)
      end if

      if ((domain % blocklist % mesh % meshScalingDel4 % ioinfo % output .and. output_obj % stream == OUTPUT) .or. &
          (domain % blocklist % mesh % meshScalingDel4 % ioinfo % restart .and. output_obj % stream == RESTART) .or. &
          (domain % blocklist % mesh % meshScalingDel4 % ioinfo % sfc .and. output_obj % stream == SFC)) then
         call MPAS_streamAddField(output_obj % io_stream, domain % blocklist % mesh % meshScalingDel4, ierr)
      end if

      if ((domain % blocklist % mesh % meshScaling % ioinfo % output .and. output_obj % stream == OUTPUT) .or. &
          (domain % blocklist % mesh % meshScaling % ioinfo % restart .and. output_obj % stream == RESTART) .or. &
          (domain % blocklist % mesh % meshScaling % ioinfo % sfc .and. output_obj % stream == SFC)) then
         call MPAS_streamAddField(output_obj % io_stream, domain % blocklist % mesh % meshScaling, ierr)
      end if

      if ((domain % blocklist % mesh % cellsOnEdge % ioinfo % output .and. output_obj % stream == OUTPUT) .or. &
          (domain % blocklist % mesh % cellsOnEdge % ioinfo % restart .and. output_obj % stream == RESTART) .or. &
          (domain % blocklist % mesh % cellsOnEdge % ioinfo % sfc .and. output_obj % stream == SFC)) then
         call MPAS_streamAddField(output_obj % io_stream, domain % blocklist % mesh % cellsOnEdge, ierr)
      end if

      if ((domain % blocklist % mesh % nEdgesOnCell % ioinfo % output .and. output_obj % stream == OUTPUT) .or. &
          (domain % blocklist % mesh % nEdgesOnCell % ioinfo % restart .and. output_obj % stream == RESTART) .or. &
          (domain % blocklist % mesh % nEdgesOnCell % ioinfo % sfc .and. output_obj % stream == SFC)) then
         call MPAS_streamAddField(output_obj % io_stream, domain % blocklist % mesh % nEdgesOnCell, ierr)
      end if

      if ((domain % blocklist % mesh % nEdgesOnEdge % ioinfo % output .and. output_obj % stream == OUTPUT) .or. &
          (domain % blocklist % mesh % nEdgesOnEdge % ioinfo % restart .and. output_obj % stream == RESTART) .or. &
          (domain % blocklist % mesh % nEdgesOnEdge % ioinfo % sfc .and. output_obj % stream == SFC)) then
         call MPAS_streamAddField(output_obj % io_stream, domain % blocklist % mesh % nEdgesOnEdge, ierr)
      end if

      if ((domain % blocklist % mesh % edgesOnCell % ioinfo % output .and. output_obj % stream == OUTPUT) .or. &
          (domain % blocklist % mesh % edgesOnCell % ioinfo % restart .and. output_obj % stream == RESTART) .or. &
          (domain % blocklist % mesh % edgesOnCell % ioinfo % sfc .and. output_obj % stream == SFC)) then
         call MPAS_streamAddField(output_obj % io_stream, domain % blocklist % mesh % edgesOnCell, ierr)
      end if

      if ((domain % blocklist % mesh % edgesOnEdge % ioinfo % output .and. output_obj % stream == OUTPUT) .or. &
          (domain % blocklist % mesh % edgesOnEdge % ioinfo % restart .and. output_obj % stream == RESTART) .or. &
          (domain % blocklist % mesh % edgesOnEdge % ioinfo % sfc .and. output_obj % stream == SFC)) then
         call MPAS_streamAddField(output_obj % io_stream, domain % blocklist % mesh % edgesOnEdge, ierr)
      end if

      if ((domain % blocklist % mesh % weightsOnEdge % ioinfo % output .and. output_obj % stream == OUTPUT) .or. &
          (domain % blocklist % mesh % weightsOnEdge % ioinfo % restart .and. output_obj % stream == RESTART) .or. &
          (domain % blocklist % mesh % weightsOnEdge % ioinfo % sfc .and. output_obj % stream == SFC)) then
         call MPAS_streamAddField(output_obj % io_stream, domain % blocklist % mesh % weightsOnEdge, ierr)
      end if

      if ((domain % blocklist % mesh % dvEdge % ioinfo % output .and. output_obj % stream == OUTPUT) .or. &
          (domain % blocklist % mesh % dvEdge % ioinfo % restart .and. output_obj % stream == RESTART) .or. &
          (domain % blocklist % mesh % dvEdge % ioinfo % sfc .and. output_obj % stream == SFC)) then
         call MPAS_streamAddField(output_obj % io_stream, domain % blocklist % mesh % dvEdge, ierr)
      end if

      if ((domain % blocklist % mesh % dcEdge % ioinfo % output .and. output_obj % stream == OUTPUT) .or. &
          (domain % blocklist % mesh % dcEdge % ioinfo % restart .and. output_obj % stream == RESTART) .or. &
          (domain % blocklist % mesh % dcEdge % ioinfo % sfc .and. output_obj % stream == SFC)) then
         call MPAS_streamAddField(output_obj % io_stream, domain % blocklist % mesh % dcEdge, ierr)
      end if

      if ((domain % blocklist % mesh % angleEdge % ioinfo % output .and. output_obj % stream == OUTPUT) .or. &
          (domain % blocklist % mesh % angleEdge % ioinfo % restart .and. output_obj % stream == RESTART) .or. &
          (domain % blocklist % mesh % angleEdge % ioinfo % sfc .and. output_obj % stream == SFC)) then
         call MPAS_streamAddField(output_obj % io_stream, domain % blocklist % mesh % angleEdge, ierr)
      end if

      if ((domain % blocklist % mesh % areaCell % ioinfo % output .and. output_obj % stream == OUTPUT) .or. &
          (domain % blocklist % mesh % areaCell % ioinfo % restart .and. output_obj % stream == RESTART) .or. &
          (domain % blocklist % mesh % areaCell % ioinfo % sfc .and. output_obj % stream == SFC)) then
         call MPAS_streamAddField(output_obj % io_stream, domain % blocklist % mesh % areaCell, ierr)
      end if

      if ((domain % blocklist % mesh % areaTriangle % ioinfo % output .and. output_obj % stream == OUTPUT) .or. &
          (domain % blocklist % mesh % areaTriangle % ioinfo % restart .and. output_obj % stream == RESTART) .or. &
          (domain % blocklist % mesh % areaTriangle % ioinfo % sfc .and. output_obj % stream == SFC)) then
         call MPAS_streamAddField(output_obj % io_stream, domain % blocklist % mesh % areaTriangle, ierr)
      end if

      if ((domain % blocklist % mesh % edgeNormalVectors % ioinfo % output .and. output_obj % stream == OUTPUT) .or. &
          (domain % blocklist % mesh % edgeNormalVectors % ioinfo % restart .and. output_obj % stream == RESTART) .or. &
          (domain % blocklist % mesh % edgeNormalVectors % ioinfo % sfc .and. output_obj % stream == SFC)) then
         call MPAS_streamAddField(output_obj % io_stream, domain % blocklist % mesh % edgeNormalVectors, ierr)
      end if

      if ((domain % blocklist % mesh % edgeTangentVectors % ioinfo % output .and. output_obj % stream == OUTPUT) .or. &
          (domain % blocklist % mesh % edgeTangentVectors % ioinfo % restart .and. output_obj % stream == RESTART) .or. &
          (domain % blocklist % mesh % edgeTangentVectors % ioinfo % sfc .and. output_obj % stream == SFC)) then
         call MPAS_streamAddField(output_obj % io_stream, domain % blocklist % mesh % edgeTangentVectors, ierr)
      end if

      if ((domain % blocklist % mesh % localVerticalUnitVectors % ioinfo % output .and. output_obj % stream == OUTPUT) .or. &
          (domain % blocklist % mesh % localVerticalUnitVectors % ioinfo % restart .and. output_obj % stream == RESTART) .or. &
          (domain % blocklist % mesh % localVerticalUnitVectors % ioinfo % sfc .and. output_obj % stream == SFC)) then
         call MPAS_streamAddField(output_obj % io_stream, domain % blocklist % mesh % localVerticalUnitVectors, ierr)
      end if

      if ((domain % blocklist % mesh % cellTangentPlane % ioinfo % output .and. output_obj % stream == OUTPUT) .or. &
          (domain % blocklist % mesh % cellTangentPlane % ioinfo % restart .and. output_obj % stream == RESTART) .or. &
          (domain % blocklist % mesh % cellTangentPlane % ioinfo % sfc .and. output_obj % stream == SFC)) then
         call MPAS_streamAddField(output_obj % io_stream, domain % blocklist % mesh % cellTangentPlane, ierr)
      end if

      if ((domain % blocklist % mesh % cellsOnCell % ioinfo % output .and. output_obj % stream == OUTPUT) .or. &
          (domain % blocklist % mesh % cellsOnCell % ioinfo % restart .and. output_obj % stream == RESTART) .or. &
          (domain % blocklist % mesh % cellsOnCell % ioinfo % sfc .and. output_obj % stream == SFC)) then
         call MPAS_streamAddField(output_obj % io_stream, domain % blocklist % mesh % cellsOnCell, ierr)
      end if

      if ((domain % blocklist % mesh % verticesOnCell % ioinfo % output .and. output_obj % stream == OUTPUT) .or. &
          (domain % blocklist % mesh % verticesOnCell % ioinfo % restart .and. output_obj % stream == RESTART) .or. &
          (domain % blocklist % mesh % verticesOnCell % ioinfo % sfc .and. output_obj % stream == SFC)) then
         call MPAS_streamAddField(output_obj % io_stream, domain % blocklist % mesh % verticesOnCell, ierr)
      end if

      if ((domain % blocklist % mesh % verticesOnEdge % ioinfo % output .and. output_obj % stream == OUTPUT) .or. &
          (domain % blocklist % mesh % verticesOnEdge % ioinfo % restart .and. output_obj % stream == RESTART) .or. &
          (domain % blocklist % mesh % verticesOnEdge % ioinfo % sfc .and. output_obj % stream == SFC)) then
         call MPAS_streamAddField(output_obj % io_stream, domain % blocklist % mesh % verticesOnEdge, ierr)
      end if

      if ((domain % blocklist % mesh % edgesOnVertex % ioinfo % output .and. output_obj % stream == OUTPUT) .or. &
          (domain % blocklist % mesh % edgesOnVertex % ioinfo % restart .and. output_obj % stream == RESTART) .or. &
          (domain % blocklist % mesh % edgesOnVertex % ioinfo % sfc .and. output_obj % stream == SFC)) then
         call MPAS_streamAddField(output_obj % io_stream, domain % blocklist % mesh % edgesOnVertex, ierr)
      end if

      if ((domain % blocklist % mesh % cellsOnVertex % ioinfo % output .and. output_obj % stream == OUTPUT) .or. &
          (domain % blocklist % mesh % cellsOnVertex % ioinfo % restart .and. output_obj % stream == RESTART) .or. &
          (domain % blocklist % mesh % cellsOnVertex % ioinfo % sfc .and. output_obj % stream == SFC)) then
         call MPAS_streamAddField(output_obj % io_stream, domain % blocklist % mesh % cellsOnVertex, ierr)
      end if

      if ((domain % blocklist % mesh % kiteAreasOnVertex % ioinfo % output .and. output_obj % stream == OUTPUT) .or. &
          (domain % blocklist % mesh % kiteAreasOnVertex % ioinfo % restart .and. output_obj % stream == RESTART) .or. &
          (domain % blocklist % mesh % kiteAreasOnVertex % ioinfo % sfc .and. output_obj % stream == SFC)) then
         call MPAS_streamAddField(output_obj % io_stream, domain % blocklist % mesh % kiteAreasOnVertex, ierr)
      end if

      if ((domain % blocklist % mesh % fEdge % ioinfo % output .and. output_obj % stream == OUTPUT) .or. &
          (domain % blocklist % mesh % fEdge % ioinfo % restart .and. output_obj % stream == RESTART) .or. &
          (domain % blocklist % mesh % fEdge % ioinfo % sfc .and. output_obj % stream == SFC)) then
         call MPAS_streamAddField(output_obj % io_stream, domain % blocklist % mesh % fEdge, ierr)
      end if

      if ((domain % blocklist % mesh % fVertex % ioinfo % output .and. output_obj % stream == OUTPUT) .or. &
          (domain % blocklist % mesh % fVertex % ioinfo % restart .and. output_obj % stream == RESTART) .or. &
          (domain % blocklist % mesh % fVertex % ioinfo % sfc .and. output_obj % stream == SFC)) then
         call MPAS_streamAddField(output_obj % io_stream, domain % blocklist % mesh % fVertex, ierr)
      end if

      if ((domain % blocklist % mesh % fCell % ioinfo % output .and. output_obj % stream == OUTPUT) .or. &
          (domain % blocklist % mesh % fCell % ioinfo % restart .and. output_obj % stream == RESTART) .or. &
          (domain % blocklist % mesh % fCell % ioinfo % sfc .and. output_obj % stream == SFC)) then
         call MPAS_streamAddField(output_obj % io_stream, domain % blocklist % mesh % fCell, ierr)
      end if

      if ((domain % blocklist % mesh % bottomDepth % ioinfo % output .and. output_obj % stream == OUTPUT) .or. &
          (domain % blocklist % mesh % bottomDepth % ioinfo % restart .and. output_obj % stream == RESTART) .or. &
          (domain % blocklist % mesh % bottomDepth % ioinfo % sfc .and. output_obj % stream == SFC)) then
         call MPAS_streamAddField(output_obj % io_stream, domain % blocklist % mesh % bottomDepth, ierr)
      end if

      if ((domain % blocklist % mesh % derivTwo % ioinfo % output .and. output_obj % stream == OUTPUT) .or. &
          (domain % blocklist % mesh % derivTwo % ioinfo % restart .and. output_obj % stream == RESTART) .or. &
          (domain % blocklist % mesh % derivTwo % ioinfo % sfc .and. output_obj % stream == SFC)) then
         call MPAS_streamAddField(output_obj % io_stream, domain % blocklist % mesh % derivTwo, ierr)
      end if

      if ((domain % blocklist % mesh % advCoefs % ioinfo % output .and. output_obj % stream == OUTPUT) .or. &
          (domain % blocklist % mesh % advCoefs % ioinfo % restart .and. output_obj % stream == RESTART) .or. &
          (domain % blocklist % mesh % advCoefs % ioinfo % sfc .and. output_obj % stream == SFC)) then
         call MPAS_streamAddField(output_obj % io_stream, domain % blocklist % mesh % advCoefs, ierr)
      end if

      if ((domain % blocklist % mesh % advCoefs3rd % ioinfo % output .and. output_obj % stream == OUTPUT) .or. &
          (domain % blocklist % mesh % advCoefs3rd % ioinfo % restart .and. output_obj % stream == RESTART) .or. &
          (domain % blocklist % mesh % advCoefs3rd % ioinfo % sfc .and. output_obj % stream == SFC)) then
         call MPAS_streamAddField(output_obj % io_stream, domain % blocklist % mesh % advCoefs3rd, ierr)
      end if

      if ((domain % blocklist % mesh % advCellsForEdge % ioinfo % output .and. output_obj % stream == OUTPUT) .or. &
          (domain % blocklist % mesh % advCellsForEdge % ioinfo % restart .and. output_obj % stream == RESTART) .or. &
          (domain % blocklist % mesh % advCellsForEdge % ioinfo % sfc .and. output_obj % stream == SFC)) then
         call MPAS_streamAddField(output_obj % io_stream, domain % blocklist % mesh % advCellsForEdge, ierr)
      end if

      if ((domain % blocklist % mesh % nAdvCellsForEdge % ioinfo % output .and. output_obj % stream == OUTPUT) .or. &
          (domain % blocklist % mesh % nAdvCellsForEdge % ioinfo % restart .and. output_obj % stream == RESTART) .or. &
          (domain % blocklist % mesh % nAdvCellsForEdge % ioinfo % sfc .and. output_obj % stream == SFC)) then
         call MPAS_streamAddField(output_obj % io_stream, domain % blocklist % mesh % nAdvCellsForEdge, ierr)
      end if

      if ((domain % blocklist % mesh % highOrderAdvectionMask % ioinfo % output .and. output_obj % stream == OUTPUT) .or. &
          (domain % blocklist % mesh % highOrderAdvectionMask % ioinfo % restart .and. output_obj % stream == RESTART) .or. &
          (domain % blocklist % mesh % highOrderAdvectionMask % ioinfo % sfc .and. output_obj % stream == SFC)) then
         call MPAS_streamAddField(output_obj % io_stream, domain % blocklist % mesh % highOrderAdvectionMask, ierr)
      end if

      if ((domain % blocklist % mesh % coeffs_reconstruct % ioinfo % output .and. output_obj % stream == OUTPUT) .or. &
          (domain % blocklist % mesh % coeffs_reconstruct % ioinfo % restart .and. output_obj % stream == RESTART) .or. &
          (domain % blocklist % mesh % coeffs_reconstruct % ioinfo % sfc .and. output_obj % stream == SFC)) then
         call MPAS_streamAddField(output_obj % io_stream, domain % blocklist % mesh % coeffs_reconstruct, ierr)
      end if

      if ((domain % blocklist % mesh % maxLevelCell % ioinfo % output .and. output_obj % stream == OUTPUT) .or. &
          (domain % blocklist % mesh % maxLevelCell % ioinfo % restart .and. output_obj % stream == RESTART) .or. &
          (domain % blocklist % mesh % maxLevelCell % ioinfo % sfc .and. output_obj % stream == SFC)) then
         call MPAS_streamAddField(output_obj % io_stream, domain % blocklist % mesh % maxLevelCell, ierr)
      end if

      if ((domain % blocklist % mesh % maxLevelEdgeTop % ioinfo % output .and. output_obj % stream == OUTPUT) .or. &
          (domain % blocklist % mesh % maxLevelEdgeTop % ioinfo % restart .and. output_obj % stream == RESTART) .or. &
          (domain % blocklist % mesh % maxLevelEdgeTop % ioinfo % sfc .and. output_obj % stream == SFC)) then
         call MPAS_streamAddField(output_obj % io_stream, domain % blocklist % mesh % maxLevelEdgeTop, ierr)
      end if

      if ((domain % blocklist % mesh % maxLevelEdgeBot % ioinfo % output .and. output_obj % stream == OUTPUT) .or. &
          (domain % blocklist % mesh % maxLevelEdgeBot % ioinfo % restart .and. output_obj % stream == RESTART) .or. &
          (domain % blocklist % mesh % maxLevelEdgeBot % ioinfo % sfc .and. output_obj % stream == SFC)) then
         call MPAS_streamAddField(output_obj % io_stream, domain % blocklist % mesh % maxLevelEdgeBot, ierr)
      end if

      if ((domain % blocklist % mesh % maxLevelVertexTop % ioinfo % output .and. output_obj % stream == OUTPUT) .or. &
          (domain % blocklist % mesh % maxLevelVertexTop % ioinfo % restart .and. output_obj % stream == RESTART) .or. &
          (domain % blocklist % mesh % maxLevelVertexTop % ioinfo % sfc .and. output_obj % stream == SFC)) then
         call MPAS_streamAddField(output_obj % io_stream, domain % blocklist % mesh % maxLevelVertexTop, ierr)
      end if

      if ((domain % blocklist % mesh % maxLevelVertexBot % ioinfo % output .and. output_obj % stream == OUTPUT) .or. &
          (domain % blocklist % mesh % maxLevelVertexBot % ioinfo % restart .and. output_obj % stream == RESTART) .or. &
          (domain % blocklist % mesh % maxLevelVertexBot % ioinfo % sfc .and. output_obj % stream == SFC)) then
         call MPAS_streamAddField(output_obj % io_stream, domain % blocklist % mesh % maxLevelVertexBot, ierr)
      end if

      if ((domain % blocklist % mesh % refBottomDepth % ioinfo % output .and. output_obj % stream == OUTPUT) .or. &
          (domain % blocklist % mesh % refBottomDepth % ioinfo % restart .and. output_obj % stream == RESTART) .or. &
          (domain % blocklist % mesh % refBottomDepth % ioinfo % sfc .and. output_obj % stream == SFC)) then
         call MPAS_streamAddField(output_obj % io_stream, domain % blocklist % mesh % refBottomDepth, ierr)
      end if

      if ((domain % blocklist % mesh % refBottomDepthTopOfCell % ioinfo % output .and. output_obj % stream == OUTPUT) .or. &
          (domain % blocklist % mesh % refBottomDepthTopOfCell % ioinfo % restart .and. output_obj % stream == RESTART) .or. &
          (domain % blocklist % mesh % refBottomDepthTopOfCell % ioinfo % sfc .and. output_obj % stream == SFC)) then
         call MPAS_streamAddField(output_obj % io_stream, domain % blocklist % mesh % refBottomDepthTopOfCell, ierr)
      end if

      if ((domain % blocklist % mesh % vertCoordMovementWeights % ioinfo % output .and. output_obj % stream == OUTPUT) .or. &
          (domain % blocklist % mesh % vertCoordMovementWeights % ioinfo % restart .and. output_obj % stream == RESTART) .or. &
          (domain % blocklist % mesh % vertCoordMovementWeights % ioinfo % sfc .and. output_obj % stream == SFC)) then
         call MPAS_streamAddField(output_obj % io_stream, domain % blocklist % mesh % vertCoordMovementWeights, ierr)
      end if

      if ((domain % blocklist % mesh % boundaryEdge % ioinfo % output .and. output_obj % stream == OUTPUT) .or. &
          (domain % blocklist % mesh % boundaryEdge % ioinfo % restart .and. output_obj % stream == RESTART) .or. &
          (domain % blocklist % mesh % boundaryEdge % ioinfo % sfc .and. output_obj % stream == SFC)) then
         call MPAS_streamAddField(output_obj % io_stream, domain % blocklist % mesh % boundaryEdge, ierr)
      end if

      if ((domain % blocklist % mesh % boundaryVertex % ioinfo % output .and. output_obj % stream == OUTPUT) .or. &
          (domain % blocklist % mesh % boundaryVertex % ioinfo % restart .and. output_obj % stream == RESTART) .or. &
          (domain % blocklist % mesh % boundaryVertex % ioinfo % sfc .and. output_obj % stream == SFC)) then
         call MPAS_streamAddField(output_obj % io_stream, domain % blocklist % mesh % boundaryVertex, ierr)
      end if

      if ((domain % blocklist % mesh % boundaryCell % ioinfo % output .and. output_obj % stream == OUTPUT) .or. &
          (domain % blocklist % mesh % boundaryCell % ioinfo % restart .and. output_obj % stream == RESTART) .or. &
          (domain % blocklist % mesh % boundaryCell % ioinfo % sfc .and. output_obj % stream == SFC)) then
         call MPAS_streamAddField(output_obj % io_stream, domain % blocklist % mesh % boundaryCell, ierr)
      end if

      if ((domain % blocklist % mesh % edgeMask % ioinfo % output .and. output_obj % stream == OUTPUT) .or. &
          (domain % blocklist % mesh % edgeMask % ioinfo % restart .and. output_obj % stream == RESTART) .or. &
          (domain % blocklist % mesh % edgeMask % ioinfo % sfc .and. output_obj % stream == SFC)) then
         call MPAS_streamAddField(output_obj % io_stream, domain % blocklist % mesh % edgeMask, ierr)
      end if

      if ((domain % blocklist % mesh % vertexMask % ioinfo % output .and. output_obj % stream == OUTPUT) .or. &
          (domain % blocklist % mesh % vertexMask % ioinfo % restart .and. output_obj % stream == RESTART) .or. &
          (domain % blocklist % mesh % vertexMask % ioinfo % sfc .and. output_obj % stream == SFC)) then
         call MPAS_streamAddField(output_obj % io_stream, domain % blocklist % mesh % vertexMask, ierr)
      end if

      if ((domain % blocklist % mesh % cellMask % ioinfo % output .and. output_obj % stream == OUTPUT) .or. &
          (domain % blocklist % mesh % cellMask % ioinfo % restart .and. output_obj % stream == RESTART) .or. &
          (domain % blocklist % mesh % cellMask % ioinfo % sfc .and. output_obj % stream == SFC)) then
         call MPAS_streamAddField(output_obj % io_stream, domain % blocklist % mesh % cellMask, ierr)
      end if

      if ((domain % blocklist % mesh % temperatureRestore % ioinfo % output .and. output_obj % stream == OUTPUT) .or. &
          (domain % blocklist % mesh % temperatureRestore % ioinfo % restart .and. output_obj % stream == RESTART) .or. &
          (domain % blocklist % mesh % temperatureRestore % ioinfo % sfc .and. output_obj % stream == SFC)) then
         call MPAS_streamAddField(output_obj % io_stream, domain % blocklist % mesh % temperatureRestore, ierr)
      end if

      if ((domain % blocklist % mesh % salinityRestore % ioinfo % output .and. output_obj % stream == OUTPUT) .or. &
          (domain % blocklist % mesh % salinityRestore % ioinfo % restart .and. output_obj % stream == RESTART) .or. &
          (domain % blocklist % mesh % salinityRestore % ioinfo % sfc .and. output_obj % stream == SFC)) then
         call MPAS_streamAddField(output_obj % io_stream, domain % blocklist % mesh % salinityRestore, ierr)
      end if

      if ((domain % blocklist % mesh % edgeSignOnCell % ioinfo % output .and. output_obj % stream == OUTPUT) .or. &
          (domain % blocklist % mesh % edgeSignOnCell % ioinfo % restart .and. output_obj % stream == RESTART) .or. &
          (domain % blocklist % mesh % edgeSignOnCell % ioinfo % sfc .and. output_obj % stream == SFC)) then
         call MPAS_streamAddField(output_obj % io_stream, domain % blocklist % mesh % edgeSignOnCell, ierr)
      end if

      if ((domain % blocklist % mesh % edgeSignOnVertex % ioinfo % output .and. output_obj % stream == OUTPUT) .or. &
          (domain % blocklist % mesh % edgeSignOnVertex % ioinfo % restart .and. output_obj % stream == RESTART) .or. &
          (domain % blocklist % mesh % edgeSignOnVertex % ioinfo % sfc .and. output_obj % stream == SFC)) then
         call MPAS_streamAddField(output_obj % io_stream, domain % blocklist % mesh % edgeSignOnVertex, ierr)
      end if

      if ((domain % blocklist % mesh % kiteIndexOnCell % ioinfo % output .and. output_obj % stream == OUTPUT) .or. &
          (domain % blocklist % mesh % kiteIndexOnCell % ioinfo % restart .and. output_obj % stream == RESTART) .or. &
          (domain % blocklist % mesh % kiteIndexOnCell % ioinfo % sfc .and. output_obj % stream == SFC)) then
         call MPAS_streamAddField(output_obj % io_stream, domain % blocklist % mesh % kiteIndexOnCell, ierr)
      end if

      if ((domain % blocklist % verticalMesh % restingThickness % ioinfo % output .and. output_obj % stream == OUTPUT) .or. &
          (domain % blocklist % verticalMesh % restingThickness % ioinfo % restart .and. output_obj % stream == RESTART) .or. &
          (domain % blocklist % verticalMesh % restingThickness % ioinfo % sfc .and. output_obj % stream == SFC)) then
         call MPAS_streamAddField(output_obj % io_stream, domain % blocklist % verticalMesh % restingThickness, ierr)
      end if

      if ((domain % blocklist % verticalMesh % refZMid % ioinfo % output .and. output_obj % stream == OUTPUT) .or. &
          (domain % blocklist % verticalMesh % refZMid % ioinfo % restart .and. output_obj % stream == RESTART) .or. &
          (domain % blocklist % verticalMesh % refZMid % ioinfo % sfc .and. output_obj % stream == SFC)) then
         call MPAS_streamAddField(output_obj % io_stream, domain % blocklist % verticalMesh % refZMid, ierr)
      end if

      if ((domain % blocklist % verticalMesh % refLayerThickness % ioinfo % output .and. output_obj % stream == OUTPUT) .or. &
          (domain % blocklist % verticalMesh % refLayerThickness % ioinfo % restart .and. output_obj % stream == RESTART) .or. &
          (domain % blocklist % verticalMesh % refLayerThickness % ioinfo % sfc .and. output_obj % stream == SFC)) then
         call MPAS_streamAddField(output_obj % io_stream, domain % blocklist % verticalMesh % refLayerThickness, ierr)
      end if

      if ((domain % blocklist % tend % tracers % ioinfo % output .and. output_obj % stream == OUTPUT) .or. &
          (domain % blocklist % tend % tracers % ioinfo % restart .and. output_obj % stream == RESTART) .or. &
          (domain % blocklist % tend % tracers % ioinfo % sfc .and. output_obj % stream == SFC)) then
         call MPAS_streamAddField(output_obj % io_stream, domain % blocklist % tend % tracers, ierr)
      end if

      if ((domain % blocklist % tend % normalVelocity % ioinfo % output .and. output_obj % stream == OUTPUT) .or. &
          (domain % blocklist % tend % normalVelocity % ioinfo % restart .and. output_obj % stream == RESTART) .or. &
          (domain % blocklist % tend % normalVelocity % ioinfo % sfc .and. output_obj % stream == SFC)) then
         call MPAS_streamAddField(output_obj % io_stream, domain % blocklist % tend % normalVelocity, ierr)
      end if

      if ((domain % blocklist % tend % layerThickness % ioinfo % output .and. output_obj % stream == OUTPUT) .or. &
          (domain % blocklist % tend % layerThickness % ioinfo % restart .and. output_obj % stream == RESTART) .or. &
          (domain % blocklist % tend % layerThickness % ioinfo % sfc .and. output_obj % stream == SFC)) then
         call MPAS_streamAddField(output_obj % io_stream, domain % blocklist % tend % layerThickness, ierr)
      end if

      if ((domain % blocklist % tend % ssh % ioinfo % output .and. output_obj % stream == OUTPUT) .or. &
          (domain % blocklist % tend % ssh % ioinfo % restart .and. output_obj % stream == RESTART) .or. &
          (domain % blocklist % tend % ssh % ioinfo % sfc .and. output_obj % stream == SFC)) then
         call MPAS_streamAddField(output_obj % io_stream, domain % blocklist % tend % ssh, ierr)
      end if

      if ((domain % blocklist % tend % highFreqThickness % ioinfo % output .and. output_obj % stream == OUTPUT) .or. &
          (domain % blocklist % tend % highFreqThickness % ioinfo % restart .and. output_obj % stream == RESTART) .or. &
          (domain % blocklist % tend % highFreqThickness % ioinfo % sfc .and. output_obj % stream == SFC)) then
         call MPAS_streamAddField(output_obj % io_stream, domain % blocklist % tend % highFreqThickness, ierr)
      end if

      if ((domain % blocklist % tend % lowFreqDivergence % ioinfo % output .and. output_obj % stream == OUTPUT) .or. &
          (domain % blocklist % tend % lowFreqDivergence % ioinfo % restart .and. output_obj % stream == RESTART) .or. &
          (domain % blocklist % tend % lowFreqDivergence % ioinfo % sfc .and. output_obj % stream == SFC)) then
         call MPAS_streamAddField(output_obj % io_stream, domain % blocklist % tend % lowFreqDivergence, ierr)
      end if

      if ((domain % blocklist % diagnostics % tracersSurfaceValue % ioinfo % output .and. output_obj % stream == OUTPUT) .or. &
          (domain % blocklist % diagnostics % tracersSurfaceValue % ioinfo % restart .and. output_obj % stream == RESTART) .or. &
          (domain % blocklist % diagnostics % tracersSurfaceValue % ioinfo % sfc .and. output_obj % stream == SFC)) then
         call MPAS_streamAddField(output_obj % io_stream, domain % blocklist % diagnostics % tracersSurfaceValue, ierr)
      end if

      if ((domain % blocklist % diagnostics % surfaceVelocity % ioinfo % output .and. output_obj % stream == OUTPUT) .or. &
          (domain % blocklist % diagnostics % surfaceVelocity % ioinfo % restart .and. output_obj % stream == RESTART) .or. &
          (domain % blocklist % diagnostics % surfaceVelocity % ioinfo % sfc .and. output_obj % stream == SFC)) then
         call MPAS_streamAddField(output_obj % io_stream, domain % blocklist % diagnostics % surfaceVelocity, ierr)
      end if

      if ((domain % blocklist % diagnostics % SSHGradient % ioinfo % output .and. output_obj % stream == OUTPUT) .or. &
          (domain % blocklist % diagnostics % SSHGradient % ioinfo % restart .and. output_obj % stream == RESTART) .or. &
          (domain % blocklist % diagnostics % SSHGradient % ioinfo % sfc .and. output_obj % stream == SFC)) then
         call MPAS_streamAddField(output_obj % io_stream, domain % blocklist % diagnostics % SSHGradient, ierr)
      end if

      if ((domain % blocklist % diagnostics % xtime % ioinfo % output .and. output_obj % stream == OUTPUT) .or. &
          (domain % blocklist % diagnostics % xtime % ioinfo % restart .and. output_obj % stream == RESTART) .or. &
          (domain % blocklist % diagnostics % xtime % ioinfo % sfc .and. output_obj % stream == SFC)) then
         call MPAS_streamAddField(output_obj % io_stream, domain % blocklist % diagnostics % xtime, ierr)
      end if

      if ((domain % blocklist % diagnostics % zMid % ioinfo % output .and. output_obj % stream == OUTPUT) .or. &
          (domain % blocklist % diagnostics % zMid % ioinfo % restart .and. output_obj % stream == RESTART) .or. &
          (domain % blocklist % diagnostics % zMid % ioinfo % sfc .and. output_obj % stream == SFC)) then
         call MPAS_streamAddField(output_obj % io_stream, domain % blocklist % diagnostics % zMid, ierr)
      end if

      if ((domain % blocklist % diagnostics % zTop % ioinfo % output .and. output_obj % stream == OUTPUT) .or. &
          (domain % blocklist % diagnostics % zTop % ioinfo % restart .and. output_obj % stream == RESTART) .or. &
          (domain % blocklist % diagnostics % zTop % ioinfo % sfc .and. output_obj % stream == SFC)) then
         call MPAS_streamAddField(output_obj % io_stream, domain % blocklist % diagnostics % zTop, ierr)
      end if

      if ((domain % blocklist % diagnostics % density % ioinfo % output .and. output_obj % stream == OUTPUT) .or. &
          (domain % blocklist % diagnostics % density % ioinfo % restart .and. output_obj % stream == RESTART) .or. &
          (domain % blocklist % diagnostics % density % ioinfo % sfc .and. output_obj % stream == SFC)) then
         call MPAS_streamAddField(output_obj % io_stream, domain % blocklist % diagnostics % density, ierr)
      end if

      if ((domain % blocklist % diagnostics % displacedDensity % ioinfo % output .and. output_obj % stream == OUTPUT) .or. &
          (domain % blocklist % diagnostics % displacedDensity % ioinfo % restart .and. output_obj % stream == RESTART) .or. &
          (domain % blocklist % diagnostics % displacedDensity % ioinfo % sfc .and. output_obj % stream == SFC)) then
         call MPAS_streamAddField(output_obj % io_stream, domain % blocklist % diagnostics % displacedDensity, ierr)
      end if

      if ((domain % blocklist % diagnostics % potentialDensity % ioinfo % output .and. output_obj % stream == OUTPUT) .or. &
          (domain % blocklist % diagnostics % potentialDensity % ioinfo % restart .and. output_obj % stream == RESTART) .or. &
          (domain % blocklist % diagnostics % potentialDensity % ioinfo % sfc .and. output_obj % stream == SFC)) then
         call MPAS_streamAddField(output_obj % io_stream, domain % blocklist % diagnostics % potentialDensity, ierr)
      end if

      if ((domain % blocklist % diagnostics % BruntVaisalaFreqTop % ioinfo % output .and. output_obj % stream == OUTPUT) .or. &
          (domain % blocklist % diagnostics % BruntVaisalaFreqTop % ioinfo % restart .and. output_obj % stream == RESTART) .or. &
          (domain % blocklist % diagnostics % BruntVaisalaFreqTop % ioinfo % sfc .and. output_obj % stream == SFC)) then
         call MPAS_streamAddField(output_obj % io_stream, domain % blocklist % diagnostics % BruntVaisalaFreqTop, ierr)
      end if

      if ((domain % blocklist % diagnostics % montgomeryPotential % ioinfo % output .and. output_obj % stream == OUTPUT) .or. &
          (domain % blocklist % diagnostics % montgomeryPotential % ioinfo % restart .and. output_obj % stream == RESTART) .or. &
          (domain % blocklist % diagnostics % montgomeryPotential % ioinfo % sfc .and. output_obj % stream == SFC)) then
         call MPAS_streamAddField(output_obj % io_stream, domain % blocklist % diagnostics % montgomeryPotential, ierr)
      end if

      if ((domain % blocklist % diagnostics % pressure % ioinfo % output .and. output_obj % stream == OUTPUT) .or. &
          (domain % blocklist % diagnostics % pressure % ioinfo % restart .and. output_obj % stream == RESTART) .or. &
          (domain % blocklist % diagnostics % pressure % ioinfo % sfc .and. output_obj % stream == SFC)) then
         call MPAS_streamAddField(output_obj % io_stream, domain % blocklist % diagnostics % pressure, ierr)
      end if

      if ((domain % blocklist % diagnostics % uTransport % ioinfo % output .and. output_obj % stream == OUTPUT) .or. &
          (domain % blocklist % diagnostics % uTransport % ioinfo % restart .and. output_obj % stream == RESTART) .or. &
          (domain % blocklist % diagnostics % uTransport % ioinfo % sfc .and. output_obj % stream == SFC)) then
         call MPAS_streamAddField(output_obj % io_stream, domain % blocklist % diagnostics % uTransport, ierr)
      end if

      if ((domain % blocklist % diagnostics % vertTransportVelocityTop % ioinfo % output .and. output_obj % stream == OUTPUT) &
.or. &
          (domain % blocklist % diagnostics % vertTransportVelocityTop % ioinfo % restart .and. output_obj % stream == RESTART) &
.or. &
          (domain % blocklist % diagnostics % vertTransportVelocityTop % ioinfo % sfc .and. output_obj % stream == SFC)) then
         call MPAS_streamAddField(output_obj % io_stream, domain % blocklist % diagnostics % vertTransportVelocityTop, ierr)
      end if

      if ((domain % blocklist % diagnostics % vertVelocityTop % ioinfo % output .and. output_obj % stream == OUTPUT) .or. &
          (domain % blocklist % diagnostics % vertVelocityTop % ioinfo % restart .and. output_obj % stream == RESTART) .or. &
          (domain % blocklist % diagnostics % vertVelocityTop % ioinfo % sfc .and. output_obj % stream == SFC)) then
         call MPAS_streamAddField(output_obj % io_stream, domain % blocklist % diagnostics % vertVelocityTop, ierr)
      end if

      if ((domain % blocklist % diagnostics % tangentialVelocity % ioinfo % output .and. output_obj % stream == OUTPUT) .or. &
          (domain % blocklist % diagnostics % tangentialVelocity % ioinfo % restart .and. output_obj % stream == RESTART) .or. &
          (domain % blocklist % diagnostics % tangentialVelocity % ioinfo % sfc .and. output_obj % stream == SFC)) then
         call MPAS_streamAddField(output_obj % io_stream, domain % blocklist % diagnostics % tangentialVelocity, ierr)
      end if

      if ((domain % blocklist % diagnostics % layerThicknessEdge % ioinfo % output .and. output_obj % stream == OUTPUT) .or. &
          (domain % blocklist % diagnostics % layerThicknessEdge % ioinfo % restart .and. output_obj % stream == RESTART) .or. &
          (domain % blocklist % diagnostics % layerThicknessEdge % ioinfo % sfc .and. output_obj % stream == SFC)) then
         call MPAS_streamAddField(output_obj % io_stream, domain % blocklist % diagnostics % layerThicknessEdge, ierr)
      end if

      if ((domain % blocklist % diagnostics % layerThicknessVertex % ioinfo % output .and. output_obj % stream == OUTPUT) .or. &
          (domain % blocklist % diagnostics % layerThicknessVertex % ioinfo % restart .and. output_obj % stream == RESTART) .or. &
          (domain % blocklist % diagnostics % layerThicknessVertex % ioinfo % sfc .and. output_obj % stream == SFC)) then
         call MPAS_streamAddField(output_obj % io_stream, domain % blocklist % diagnostics % layerThicknessVertex, ierr)
      end if

      if ((domain % blocklist % diagnostics % kineticEnergyCell % ioinfo % output .and. output_obj % stream == OUTPUT) .or. &
          (domain % blocklist % diagnostics % kineticEnergyCell % ioinfo % restart .and. output_obj % stream == RESTART) .or. &
          (domain % blocklist % diagnostics % kineticEnergyCell % ioinfo % sfc .and. output_obj % stream == SFC)) then
         call MPAS_streamAddField(output_obj % io_stream, domain % blocklist % diagnostics % kineticEnergyCell, ierr)
      end if

      if ((domain % blocklist % diagnostics % hEddyFlux % ioinfo % output .and. output_obj % stream == OUTPUT) .or. &
          (domain % blocklist % diagnostics % hEddyFlux % ioinfo % restart .and. output_obj % stream == RESTART) .or. &
          (domain % blocklist % diagnostics % hEddyFlux % ioinfo % sfc .and. output_obj % stream == SFC)) then
         call MPAS_streamAddField(output_obj % io_stream, domain % blocklist % diagnostics % hEddyFlux, ierr)
      end if

      if ((domain % blocklist % diagnostics % hKappa % ioinfo % output .and. output_obj % stream == OUTPUT) .or. &
          (domain % blocklist % diagnostics % hKappa % ioinfo % restart .and. output_obj % stream == RESTART) .or. &
          (domain % blocklist % diagnostics % hKappa % ioinfo % sfc .and. output_obj % stream == SFC)) then
         call MPAS_streamAddField(output_obj % io_stream, domain % blocklist % diagnostics % hKappa, ierr)
      end if

      if ((domain % blocklist % diagnostics % hKappaQ % ioinfo % output .and. output_obj % stream == OUTPUT) .or. &
          (domain % blocklist % diagnostics % hKappaQ % ioinfo % restart .and. output_obj % stream == RESTART) .or. &
          (domain % blocklist % diagnostics % hKappaQ % ioinfo % sfc .and. output_obj % stream == SFC)) then
         call MPAS_streamAddField(output_obj % io_stream, domain % blocklist % diagnostics % hKappaQ, ierr)
      end if

      if ((domain % blocklist % diagnostics % viscosity % ioinfo % output .and. output_obj % stream == OUTPUT) .or. &
          (domain % blocklist % diagnostics % viscosity % ioinfo % restart .and. output_obj % stream == RESTART) .or. &
          (domain % blocklist % diagnostics % viscosity % ioinfo % sfc .and. output_obj % stream == SFC)) then
         call MPAS_streamAddField(output_obj % io_stream, domain % blocklist % diagnostics % viscosity, ierr)
      end if

      if ((domain % blocklist % diagnostics % divergence % ioinfo % output .and. output_obj % stream == OUTPUT) .or. &
          (domain % blocklist % diagnostics % divergence % ioinfo % restart .and. output_obj % stream == RESTART) .or. &
          (domain % blocklist % diagnostics % divergence % ioinfo % sfc .and. output_obj % stream == SFC)) then
         call MPAS_streamAddField(output_obj % io_stream, domain % blocklist % diagnostics % divergence, ierr)
      end if

      if ((domain % blocklist % diagnostics % circulation % ioinfo % output .and. output_obj % stream == OUTPUT) .or. &
          (domain % blocklist % diagnostics % circulation % ioinfo % restart .and. output_obj % stream == RESTART) .or. &
          (domain % blocklist % diagnostics % circulation % ioinfo % sfc .and. output_obj % stream == SFC)) then
         call MPAS_streamAddField(output_obj % io_stream, domain % blocklist % diagnostics % circulation, ierr)
      end if

      if ((domain % blocklist % diagnostics % relativeVorticity % ioinfo % output .and. output_obj % stream == OUTPUT) .or. &
          (domain % blocklist % diagnostics % relativeVorticity % ioinfo % restart .and. output_obj % stream == RESTART) .or. &
          (domain % blocklist % diagnostics % relativeVorticity % ioinfo % sfc .and. output_obj % stream == SFC)) then
         call MPAS_streamAddField(output_obj % io_stream, domain % blocklist % diagnostics % relativeVorticity, ierr)
      end if

      if ((domain % blocklist % diagnostics % relativeVorticityCell % ioinfo % output .and. output_obj % stream == OUTPUT) .or. &
          (domain % blocklist % diagnostics % relativeVorticityCell % ioinfo % restart .and. output_obj % stream == RESTART) .or. &
          (domain % blocklist % diagnostics % relativeVorticityCell % ioinfo % sfc .and. output_obj % stream == SFC)) then
         call MPAS_streamAddField(output_obj % io_stream, domain % blocklist % diagnostics % relativeVorticityCell, ierr)
      end if

      if ((domain % blocklist % diagnostics % normalizedRelativeVorticityEdge % ioinfo % output .and. output_obj % stream == &
OUTPUT) .or. &
          (domain % blocklist % diagnostics % normalizedRelativeVorticityEdge % ioinfo % restart .and. output_obj % stream == &
RESTART) .or. &
          (domain % blocklist % diagnostics % normalizedRelativeVorticityEdge % ioinfo % sfc .and. output_obj % stream == SFC)) &
then
         call MPAS_streamAddField(output_obj % io_stream, domain % blocklist % diagnostics % normalizedRelativeVorticityEdge, ierr)
      end if

      if ((domain % blocklist % diagnostics % normalizedPlanetaryVorticityEdge % ioinfo % output .and. output_obj % stream == &
OUTPUT) .or. &
          (domain % blocklist % diagnostics % normalizedPlanetaryVorticityEdge % ioinfo % restart .and. output_obj % stream == &
RESTART) .or. &
          (domain % blocklist % diagnostics % normalizedPlanetaryVorticityEdge % ioinfo % sfc .and. output_obj % stream == SFC)) &
then
         call MPAS_streamAddField(output_obj % io_stream, domain % blocklist % diagnostics % normalizedPlanetaryVorticityEdge, &
ierr)
      end if

      if ((domain % blocklist % diagnostics % normalizedRelativeVorticityCell % ioinfo % output .and. output_obj % stream == &
OUTPUT) .or. &
          (domain % blocklist % diagnostics % normalizedRelativeVorticityCell % ioinfo % restart .and. output_obj % stream == &
RESTART) .or. &
          (domain % blocklist % diagnostics % normalizedRelativeVorticityCell % ioinfo % sfc .and. output_obj % stream == SFC)) &
then
         call MPAS_streamAddField(output_obj % io_stream, domain % blocklist % diagnostics % normalizedRelativeVorticityCell, ierr)
      end if

      if ((domain % blocklist % diagnostics % barotropicForcing % ioinfo % output .and. output_obj % stream == OUTPUT) .or. &
          (domain % blocklist % diagnostics % barotropicForcing % ioinfo % restart .and. output_obj % stream == RESTART) .or. &
          (domain % blocklist % diagnostics % barotropicForcing % ioinfo % sfc .and. output_obj % stream == SFC)) then
         call MPAS_streamAddField(output_obj % io_stream, domain % blocklist % diagnostics % barotropicForcing, ierr)
      end if

      if ((domain % blocklist % diagnostics % barotropicThicknessFlux % ioinfo % output .and. output_obj % stream == OUTPUT) .or. &
          (domain % blocklist % diagnostics % barotropicThicknessFlux % ioinfo % restart .and. output_obj % stream == RESTART) &
.or. &
          (domain % blocklist % diagnostics % barotropicThicknessFlux % ioinfo % sfc .and. output_obj % stream == SFC)) then
         call MPAS_streamAddField(output_obj % io_stream, domain % blocklist % diagnostics % barotropicThicknessFlux, ierr)
      end if

      if ((domain % blocklist % diagnostics % normalVelocityX % ioinfo % output .and. output_obj % stream == OUTPUT) .or. &
          (domain % blocklist % diagnostics % normalVelocityX % ioinfo % restart .and. output_obj % stream == RESTART) .or. &
          (domain % blocklist % diagnostics % normalVelocityX % ioinfo % sfc .and. output_obj % stream == SFC)) then
         call MPAS_streamAddField(output_obj % io_stream, domain % blocklist % diagnostics % normalVelocityX, ierr)
      end if

      if ((domain % blocklist % diagnostics % normalVelocityY % ioinfo % output .and. output_obj % stream == OUTPUT) .or. &
          (domain % blocklist % diagnostics % normalVelocityY % ioinfo % restart .and. output_obj % stream == RESTART) .or. &
          (domain % blocklist % diagnostics % normalVelocityY % ioinfo % sfc .and. output_obj % stream == SFC)) then
         call MPAS_streamAddField(output_obj % io_stream, domain % blocklist % diagnostics % normalVelocityY, ierr)
      end if

      if ((domain % blocklist % diagnostics % normalVelocityZ % ioinfo % output .and. output_obj % stream == OUTPUT) .or. &
          (domain % blocklist % diagnostics % normalVelocityZ % ioinfo % restart .and. output_obj % stream == RESTART) .or. &
          (domain % blocklist % diagnostics % normalVelocityZ % ioinfo % sfc .and. output_obj % stream == SFC)) then
         call MPAS_streamAddField(output_obj % io_stream, domain % blocklist % diagnostics % normalVelocityZ, ierr)
      end if

      if ((domain % blocklist % diagnostics % normalVelocityZonal % ioinfo % output .and. output_obj % stream == OUTPUT) .or. &
          (domain % blocklist % diagnostics % normalVelocityZonal % ioinfo % restart .and. output_obj % stream == RESTART) .or. &
          (domain % blocklist % diagnostics % normalVelocityZonal % ioinfo % sfc .and. output_obj % stream == SFC)) then
         call MPAS_streamAddField(output_obj % io_stream, domain % blocklist % diagnostics % normalVelocityZonal, ierr)
      end if

      if ((domain % blocklist % diagnostics % normalVelocityMeridional % ioinfo % output .and. output_obj % stream == OUTPUT) &
.or. &
          (domain % blocklist % diagnostics % normalVelocityMeridional % ioinfo % restart .and. output_obj % stream == RESTART) &
.or. &
          (domain % blocklist % diagnostics % normalVelocityMeridional % ioinfo % sfc .and. output_obj % stream == SFC)) then
         call MPAS_streamAddField(output_obj % io_stream, domain % blocklist % diagnostics % normalVelocityMeridional, ierr)
      end if

      if ((domain % blocklist % diagnostics % gradSSH % ioinfo % output .and. output_obj % stream == OUTPUT) .or. &
          (domain % blocklist % diagnostics % gradSSH % ioinfo % restart .and. output_obj % stream == RESTART) .or. &
          (domain % blocklist % diagnostics % gradSSH % ioinfo % sfc .and. output_obj % stream == SFC)) then
         call MPAS_streamAddField(output_obj % io_stream, domain % blocklist % diagnostics % gradSSH, ierr)
      end if

      if ((domain % blocklist % diagnostics % gradSSHX % ioinfo % output .and. output_obj % stream == OUTPUT) .or. &
          (domain % blocklist % diagnostics % gradSSHX % ioinfo % restart .and. output_obj % stream == RESTART) .or. &
          (domain % blocklist % diagnostics % gradSSHX % ioinfo % sfc .and. output_obj % stream == SFC)) then
         call MPAS_streamAddField(output_obj % io_stream, domain % blocklist % diagnostics % gradSSHX, ierr)
      end if

      if ((domain % blocklist % diagnostics % gradSSHY % ioinfo % output .and. output_obj % stream == OUTPUT) .or. &
          (domain % blocklist % diagnostics % gradSSHY % ioinfo % restart .and. output_obj % stream == RESTART) .or. &
          (domain % blocklist % diagnostics % gradSSHY % ioinfo % sfc .and. output_obj % stream == SFC)) then
         call MPAS_streamAddField(output_obj % io_stream, domain % blocklist % diagnostics % gradSSHY, ierr)
      end if

      if ((domain % blocklist % diagnostics % gradSSHZ % ioinfo % output .and. output_obj % stream == OUTPUT) .or. &
          (domain % blocklist % diagnostics % gradSSHZ % ioinfo % restart .and. output_obj % stream == RESTART) .or. &
          (domain % blocklist % diagnostics % gradSSHZ % ioinfo % sfc .and. output_obj % stream == SFC)) then
         call MPAS_streamAddField(output_obj % io_stream, domain % blocklist % diagnostics % gradSSHZ, ierr)
      end if

      if ((domain % blocklist % diagnostics % gradSSHZonal % ioinfo % output .and. output_obj % stream == OUTPUT) .or. &
          (domain % blocklist % diagnostics % gradSSHZonal % ioinfo % restart .and. output_obj % stream == RESTART) .or. &
          (domain % blocklist % diagnostics % gradSSHZonal % ioinfo % sfc .and. output_obj % stream == SFC)) then
         call MPAS_streamAddField(output_obj % io_stream, domain % blocklist % diagnostics % gradSSHZonal, ierr)
      end if

      if ((domain % blocklist % diagnostics % gradSSHMeridional % ioinfo % output .and. output_obj % stream == OUTPUT) .or. &
          (domain % blocklist % diagnostics % gradSSHMeridional % ioinfo % restart .and. output_obj % stream == RESTART) .or. &
          (domain % blocklist % diagnostics % gradSSHMeridional % ioinfo % sfc .and. output_obj % stream == SFC)) then
         call MPAS_streamAddField(output_obj % io_stream, domain % blocklist % diagnostics % gradSSHMeridional, ierr)
      end if

      if ((domain % blocklist % diagnostics % uBolusGM % ioinfo % output .and. output_obj % stream == OUTPUT) .or. &
          (domain % blocklist % diagnostics % uBolusGM % ioinfo % restart .and. output_obj % stream == RESTART) .or. &
          (domain % blocklist % diagnostics % uBolusGM % ioinfo % sfc .and. output_obj % stream == SFC)) then
         call MPAS_streamAddField(output_obj % io_stream, domain % blocklist % diagnostics % uBolusGM, ierr)
      end if

      if ((domain % blocklist % diagnostics % uBolusGMX % ioinfo % output .and. output_obj % stream == OUTPUT) .or. &
          (domain % blocklist % diagnostics % uBolusGMX % ioinfo % restart .and. output_obj % stream == RESTART) .or. &
          (domain % blocklist % diagnostics % uBolusGMX % ioinfo % sfc .and. output_obj % stream == SFC)) then
         call MPAS_streamAddField(output_obj % io_stream, domain % blocklist % diagnostics % uBolusGMX, ierr)
      end if

      if ((domain % blocklist % diagnostics % uBolusGMY % ioinfo % output .and. output_obj % stream == OUTPUT) .or. &
          (domain % blocklist % diagnostics % uBolusGMY % ioinfo % restart .and. output_obj % stream == RESTART) .or. &
          (domain % blocklist % diagnostics % uBolusGMY % ioinfo % sfc .and. output_obj % stream == SFC)) then
         call MPAS_streamAddField(output_obj % io_stream, domain % blocklist % diagnostics % uBolusGMY, ierr)
      end if

      if ((domain % blocklist % diagnostics % uBolusGMZ % ioinfo % output .and. output_obj % stream == OUTPUT) .or. &
          (domain % blocklist % diagnostics % uBolusGMZ % ioinfo % restart .and. output_obj % stream == RESTART) .or. &
          (domain % blocklist % diagnostics % uBolusGMZ % ioinfo % sfc .and. output_obj % stream == SFC)) then
         call MPAS_streamAddField(output_obj % io_stream, domain % blocklist % diagnostics % uBolusGMZ, ierr)
      end if

      if ((domain % blocklist % diagnostics % uBolusGMZonal % ioinfo % output .and. output_obj % stream == OUTPUT) .or. &
          (domain % blocklist % diagnostics % uBolusGMZonal % ioinfo % restart .and. output_obj % stream == RESTART) .or. &
          (domain % blocklist % diagnostics % uBolusGMZonal % ioinfo % sfc .and. output_obj % stream == SFC)) then
         call MPAS_streamAddField(output_obj % io_stream, domain % blocklist % diagnostics % uBolusGMZonal, ierr)
      end if

      if ((domain % blocklist % diagnostics % uBolusGMMeridional % ioinfo % output .and. output_obj % stream == OUTPUT) .or. &
          (domain % blocklist % diagnostics % uBolusGMMeridional % ioinfo % restart .and. output_obj % stream == RESTART) .or. &
          (domain % blocklist % diagnostics % uBolusGMMeridional % ioinfo % sfc .and. output_obj % stream == SFC)) then
         call MPAS_streamAddField(output_obj % io_stream, domain % blocklist % diagnostics % uBolusGMMeridional, ierr)
      end if

      if ((domain % blocklist % diagnostics % RiTopOfCell % ioinfo % output .and. output_obj % stream == OUTPUT) .or. &
          (domain % blocklist % diagnostics % RiTopOfCell % ioinfo % restart .and. output_obj % stream == RESTART) .or. &
          (domain % blocklist % diagnostics % RiTopOfCell % ioinfo % sfc .and. output_obj % stream == SFC)) then
         call MPAS_streamAddField(output_obj % io_stream, domain % blocklist % diagnostics % RiTopOfCell, ierr)
      end if

      if ((domain % blocklist % diagnostics % RiTopOfEdge % ioinfo % output .and. output_obj % stream == OUTPUT) .or. &
          (domain % blocklist % diagnostics % RiTopOfEdge % ioinfo % restart .and. output_obj % stream == RESTART) .or. &
          (domain % blocklist % diagnostics % RiTopOfEdge % ioinfo % sfc .and. output_obj % stream == SFC)) then
         call MPAS_streamAddField(output_obj % io_stream, domain % blocklist % diagnostics % RiTopOfEdge, ierr)
      end if

      if ((domain % blocklist % diagnostics % vertViscTopOfEdge % ioinfo % output .and. output_obj % stream == OUTPUT) .or. &
          (domain % blocklist % diagnostics % vertViscTopOfEdge % ioinfo % restart .and. output_obj % stream == RESTART) .or. &
          (domain % blocklist % diagnostics % vertViscTopOfEdge % ioinfo % sfc .and. output_obj % stream == SFC)) then
         call MPAS_streamAddField(output_obj % io_stream, domain % blocklist % diagnostics % vertViscTopOfEdge, ierr)
      end if

      if ((domain % blocklist % diagnostics % vertViscTopOfCell % ioinfo % output .and. output_obj % stream == OUTPUT) .or. &
          (domain % blocklist % diagnostics % vertViscTopOfCell % ioinfo % restart .and. output_obj % stream == RESTART) .or. &
          (domain % blocklist % diagnostics % vertViscTopOfCell % ioinfo % sfc .and. output_obj % stream == SFC)) then
         call MPAS_streamAddField(output_obj % io_stream, domain % blocklist % diagnostics % vertViscTopOfCell, ierr)
      end if

      if ((domain % blocklist % diagnostics % vertDiffTopOfCell % ioinfo % output .and. output_obj % stream == OUTPUT) .or. &
          (domain % blocklist % diagnostics % vertDiffTopOfCell % ioinfo % restart .and. output_obj % stream == RESTART) .or. &
          (domain % blocklist % diagnostics % vertDiffTopOfCell % ioinfo % sfc .and. output_obj % stream == SFC)) then
         call MPAS_streamAddField(output_obj % io_stream, domain % blocklist % diagnostics % vertDiffTopOfCell, ierr)
      end if

      if ((domain % blocklist % diagnostics % bulkRichardsonNumber % ioinfo % output .and. output_obj % stream == OUTPUT) .or. &
          (domain % blocklist % diagnostics % bulkRichardsonNumber % ioinfo % restart .and. output_obj % stream == RESTART) .or. &
          (domain % blocklist % diagnostics % bulkRichardsonNumber % ioinfo % sfc .and. output_obj % stream == SFC)) then
         call MPAS_streamAddField(output_obj % io_stream, domain % blocklist % diagnostics % bulkRichardsonNumber, ierr)
      end if

      if ((domain % blocklist % diagnostics % boundaryLayerDepth % ioinfo % output .and. output_obj % stream == OUTPUT) .or. &
          (domain % blocklist % diagnostics % boundaryLayerDepth % ioinfo % restart .and. output_obj % stream == RESTART) .or. &
          (domain % blocklist % diagnostics % boundaryLayerDepth % ioinfo % sfc .and. output_obj % stream == SFC)) then
         call MPAS_streamAddField(output_obj % io_stream, domain % blocklist % diagnostics % boundaryLayerDepth, ierr)
      end if

      if ((domain % blocklist % diagnostics % indexBoundaryLayerDepth % ioinfo % output .and. output_obj % stream == OUTPUT) .or. &
          (domain % blocklist % diagnostics % indexBoundaryLayerDepth % ioinfo % restart .and. output_obj % stream == RESTART) &
.or. &
          (domain % blocklist % diagnostics % indexBoundaryLayerDepth % ioinfo % sfc .and. output_obj % stream == SFC)) then
         call MPAS_streamAddField(output_obj % io_stream, domain % blocklist % diagnostics % indexBoundaryLayerDepth, ierr)
      end if

      if ((domain % blocklist % diagnostics % surfaceFrictionVelocity % ioinfo % output .and. output_obj % stream == OUTPUT) .or. &
          (domain % blocklist % diagnostics % surfaceFrictionVelocity % ioinfo % restart .and. output_obj % stream == RESTART) &
.or. &
          (domain % blocklist % diagnostics % surfaceFrictionVelocity % ioinfo % sfc .and. output_obj % stream == SFC)) then
         call MPAS_streamAddField(output_obj % io_stream, domain % blocklist % diagnostics % surfaceFrictionVelocity, ierr)
      end if

      if ((domain % blocklist % diagnostics % windStressZonalDiag % ioinfo % output .and. output_obj % stream == OUTPUT) .or. &
          (domain % blocklist % diagnostics % windStressZonalDiag % ioinfo % restart .and. output_obj % stream == RESTART) .or. &
          (domain % blocklist % diagnostics % windStressZonalDiag % ioinfo % sfc .and. output_obj % stream == SFC)) then
         call MPAS_streamAddField(output_obj % io_stream, domain % blocklist % diagnostics % windStressZonalDiag, ierr)
      end if

      if ((domain % blocklist % diagnostics % windStressMeridionalDiag % ioinfo % output .and. output_obj % stream == OUTPUT) &
.or. &
          (domain % blocklist % diagnostics % windStressMeridionalDiag % ioinfo % restart .and. output_obj % stream == RESTART) &
.or. &
          (domain % blocklist % diagnostics % windStressMeridionalDiag % ioinfo % sfc .and. output_obj % stream == SFC)) then
         call MPAS_streamAddField(output_obj % io_stream, domain % blocklist % diagnostics % windStressMeridionalDiag, ierr)
      end if

      if ((domain % blocklist % diagnostics % penetrativeTemperatureFluxOBL % ioinfo % output .and. output_obj % stream == OUTPUT) &
.or. &
          (domain % blocklist % diagnostics % penetrativeTemperatureFluxOBL % ioinfo % restart .and. output_obj % stream == &
RESTART) .or. &
          (domain % blocklist % diagnostics % penetrativeTemperatureFluxOBL % ioinfo % sfc .and. output_obj % stream == SFC)) then
         call MPAS_streamAddField(output_obj % io_stream, domain % blocklist % diagnostics % penetrativeTemperatureFluxOBL, ierr)
      end if

      if ((domain % blocklist % diagnostics % buoyancyForcingOBL % ioinfo % output .and. output_obj % stream == OUTPUT) .or. &
          (domain % blocklist % diagnostics % buoyancyForcingOBL % ioinfo % restart .and. output_obj % stream == RESTART) .or. &
          (domain % blocklist % diagnostics % buoyancyForcingOBL % ioinfo % sfc .and. output_obj % stream == SFC)) then
         call MPAS_streamAddField(output_obj % io_stream, domain % blocklist % diagnostics % buoyancyForcingOBL, ierr)
      end if

      if ((domain % blocklist % diagnostics % areaCellGlobal % ioinfo % output .and. output_obj % stream == OUTPUT) .or. &
          (domain % blocklist % diagnostics % areaCellGlobal % ioinfo % restart .and. output_obj % stream == RESTART) .or. &
          (domain % blocklist % diagnostics % areaCellGlobal % ioinfo % sfc .and. output_obj % stream == SFC)) then
         call MPAS_streamAddField(output_obj % io_stream, domain % blocklist % diagnostics % areaCellGlobal, ierr)
      end if

      if ((domain % blocklist % diagnostics % areaEdgeGlobal % ioinfo % output .and. output_obj % stream == OUTPUT) .or. &
          (domain % blocklist % diagnostics % areaEdgeGlobal % ioinfo % restart .and. output_obj % stream == RESTART) .or. &
          (domain % blocklist % diagnostics % areaEdgeGlobal % ioinfo % sfc .and. output_obj % stream == SFC)) then
         call MPAS_streamAddField(output_obj % io_stream, domain % blocklist % diagnostics % areaEdgeGlobal, ierr)
      end if

      if ((domain % blocklist % diagnostics % areaTriangleGlobal % ioinfo % output .and. output_obj % stream == OUTPUT) .or. &
          (domain % blocklist % diagnostics % areaTriangleGlobal % ioinfo % restart .and. output_obj % stream == RESTART) .or. &
          (domain % blocklist % diagnostics % areaTriangleGlobal % ioinfo % sfc .and. output_obj % stream == SFC)) then
         call MPAS_streamAddField(output_obj % io_stream, domain % blocklist % diagnostics % areaTriangleGlobal, ierr)
      end if

      if ((domain % blocklist % diagnostics % volumeCellGlobal % ioinfo % output .and. output_obj % stream == OUTPUT) .or. &
          (domain % blocklist % diagnostics % volumeCellGlobal % ioinfo % restart .and. output_obj % stream == RESTART) .or. &
          (domain % blocklist % diagnostics % volumeCellGlobal % ioinfo % sfc .and. output_obj % stream == SFC)) then
         call MPAS_streamAddField(output_obj % io_stream, domain % blocklist % diagnostics % volumeCellGlobal, ierr)
      end if

      if ((domain % blocklist % diagnostics % volumeEdgeGlobal % ioinfo % output .and. output_obj % stream == OUTPUT) .or. &
          (domain % blocklist % diagnostics % volumeEdgeGlobal % ioinfo % restart .and. output_obj % stream == RESTART) .or. &
          (domain % blocklist % diagnostics % volumeEdgeGlobal % ioinfo % sfc .and. output_obj % stream == SFC)) then
         call MPAS_streamAddField(output_obj % io_stream, domain % blocklist % diagnostics % volumeEdgeGlobal, ierr)
      end if

      if ((domain % blocklist % diagnostics % CFLNumberGlobal % ioinfo % output .and. output_obj % stream == OUTPUT) .or. &
          (domain % blocklist % diagnostics % CFLNumberGlobal % ioinfo % restart .and. output_obj % stream == RESTART) .or. &
          (domain % blocklist % diagnostics % CFLNumberGlobal % ioinfo % sfc .and. output_obj % stream == SFC)) then
         call MPAS_streamAddField(output_obj % io_stream, domain % blocklist % diagnostics % CFLNumberGlobal, ierr)
      end if

      if ((domain % blocklist % average % nAverage % ioinfo % output .and. output_obj % stream == OUTPUT) .or. &
          (domain % blocklist % average % nAverage % ioinfo % restart .and. output_obj % stream == RESTART) .or. &
          (domain % blocklist % average % nAverage % ioinfo % sfc .and. output_obj % stream == SFC)) then
         call MPAS_streamAddField(output_obj % io_stream, domain % blocklist % average % nAverage, ierr)
      end if

      if ((domain % blocklist % average % avgSsh % ioinfo % output .and. output_obj % stream == OUTPUT) .or. &
          (domain % blocklist % average % avgSsh % ioinfo % restart .and. output_obj % stream == RESTART) .or. &
          (domain % blocklist % average % avgSsh % ioinfo % sfc .and. output_obj % stream == SFC)) then
         call MPAS_streamAddField(output_obj % io_stream, domain % blocklist % average % avgSsh, ierr)
      end if

      if ((domain % blocklist % average % varSsh % ioinfo % output .and. output_obj % stream == OUTPUT) .or. &
          (domain % blocklist % average % varSsh % ioinfo % restart .and. output_obj % stream == RESTART) .or. &
          (domain % blocklist % average % varSsh % ioinfo % sfc .and. output_obj % stream == SFC)) then
         call MPAS_streamAddField(output_obj % io_stream, domain % blocklist % average % varSsh, ierr)
      end if

      if ((domain % blocklist % average % avgVelocityZonal % ioinfo % output .and. output_obj % stream == OUTPUT) .or. &
          (domain % blocklist % average % avgVelocityZonal % ioinfo % restart .and. output_obj % stream == RESTART) .or. &
          (domain % blocklist % average % avgVelocityZonal % ioinfo % sfc .and. output_obj % stream == SFC)) then
         call MPAS_streamAddField(output_obj % io_stream, domain % blocklist % average % avgVelocityZonal, ierr)
      end if

      if ((domain % blocklist % average % avgVelocityMeridional % ioinfo % output .and. output_obj % stream == OUTPUT) .or. &
          (domain % blocklist % average % avgVelocityMeridional % ioinfo % restart .and. output_obj % stream == RESTART) .or. &
          (domain % blocklist % average % avgVelocityMeridional % ioinfo % sfc .and. output_obj % stream == SFC)) then
         call MPAS_streamAddField(output_obj % io_stream, domain % blocklist % average % avgVelocityMeridional, ierr)
      end if

      if ((domain % blocklist % average % varVelocityZonal % ioinfo % output .and. output_obj % stream == OUTPUT) .or. &
          (domain % blocklist % average % varVelocityZonal % ioinfo % restart .and. output_obj % stream == RESTART) .or. &
          (domain % blocklist % average % varVelocityZonal % ioinfo % sfc .and. output_obj % stream == SFC)) then
         call MPAS_streamAddField(output_obj % io_stream, domain % blocklist % average % varVelocityZonal, ierr)
      end if

      if ((domain % blocklist % average % varVelocityMeridional % ioinfo % output .and. output_obj % stream == OUTPUT) .or. &
          (domain % blocklist % average % varVelocityMeridional % ioinfo % restart .and. output_obj % stream == RESTART) .or. &
          (domain % blocklist % average % varVelocityMeridional % ioinfo % sfc .and. output_obj % stream == SFC)) then
         call MPAS_streamAddField(output_obj % io_stream, domain % blocklist % average % varVelocityMeridional, ierr)
      end if

      if ((domain % blocklist % average % avgNormalVelocity % ioinfo % output .and. output_obj % stream == OUTPUT) .or. &
          (domain % blocklist % average % avgNormalVelocity % ioinfo % restart .and. output_obj % stream == RESTART) .or. &
          (domain % blocklist % average % avgNormalVelocity % ioinfo % sfc .and. output_obj % stream == SFC)) then
         call MPAS_streamAddField(output_obj % io_stream, domain % blocklist % average % avgNormalVelocity, ierr)
      end if

      if ((domain % blocklist % average % varNormalVelocity % ioinfo % output .and. output_obj % stream == OUTPUT) .or. &
          (domain % blocklist % average % varNormalVelocity % ioinfo % restart .and. output_obj % stream == RESTART) .or. &
          (domain % blocklist % average % varNormalVelocity % ioinfo % sfc .and. output_obj % stream == SFC)) then
         call MPAS_streamAddField(output_obj % io_stream, domain % blocklist % average % varNormalVelocity, ierr)
      end if

      if ((domain % blocklist % average % avgVertVelocityTop % ioinfo % output .and. output_obj % stream == OUTPUT) .or. &
          (domain % blocklist % average % avgVertVelocityTop % ioinfo % restart .and. output_obj % stream == RESTART) .or. &
          (domain % blocklist % average % avgVertVelocityTop % ioinfo % sfc .and. output_obj % stream == SFC)) then
         call MPAS_streamAddField(output_obj % io_stream, domain % blocklist % average % avgVertVelocityTop, ierr)
      end if

      if ((domain % blocklist % forcing % surfaceTracerFlux % ioinfo % output .and. output_obj % stream == OUTPUT) .or. &
          (domain % blocklist % forcing % surfaceTracerFlux % ioinfo % restart .and. output_obj % stream == RESTART) .or. &
          (domain % blocklist % forcing % surfaceTracerFlux % ioinfo % sfc .and. output_obj % stream == SFC)) then
         call MPAS_streamAddField(output_obj % io_stream, domain % blocklist % forcing % surfaceTracerFlux, ierr)
      end if

      if ((domain % blocklist % forcing % avgTracersSurfaceValue % ioinfo % output .and. output_obj % stream == OUTPUT) .or. &
          (domain % blocklist % forcing % avgTracersSurfaceValue % ioinfo % restart .and. output_obj % stream == RESTART) .or. &
          (domain % blocklist % forcing % avgTracersSurfaceValue % ioinfo % sfc .and. output_obj % stream == SFC)) then
         call MPAS_streamAddField(output_obj % io_stream, domain % blocklist % forcing % avgTracersSurfaceValue, ierr)
      end if

      if ((domain % blocklist % forcing % avgSurfaceVelocity % ioinfo % output .and. output_obj % stream == OUTPUT) .or. &
          (domain % blocklist % forcing % avgSurfaceVelocity % ioinfo % restart .and. output_obj % stream == RESTART) .or. &
          (domain % blocklist % forcing % avgSurfaceVelocity % ioinfo % sfc .and. output_obj % stream == SFC)) then
         call MPAS_streamAddField(output_obj % io_stream, domain % blocklist % forcing % avgSurfaceVelocity, ierr)
      end if

      if ((domain % blocklist % forcing % avgSSHGradient % ioinfo % output .and. output_obj % stream == OUTPUT) .or. &
          (domain % blocklist % forcing % avgSSHGradient % ioinfo % restart .and. output_obj % stream == RESTART) .or. &
          (domain % blocklist % forcing % avgSSHGradient % ioinfo % sfc .and. output_obj % stream == SFC)) then
         call MPAS_streamAddField(output_obj % io_stream, domain % blocklist % forcing % avgSSHGradient, ierr)
      end if

      if ((domain % blocklist % forcing % surfaceWindStress % ioinfo % output .and. output_obj % stream == OUTPUT) .or. &
          (domain % blocklist % forcing % surfaceWindStress % ioinfo % restart .and. output_obj % stream == RESTART) .or. &
          (domain % blocklist % forcing % surfaceWindStress % ioinfo % sfc .and. output_obj % stream == SFC)) then
         call MPAS_streamAddField(output_obj % io_stream, domain % blocklist % forcing % surfaceWindStress, ierr)
      end if

      if ((domain % blocklist % forcing % surfaceWindStressMagnitude % ioinfo % output .and. output_obj % stream == OUTPUT) .or. &
          (domain % blocklist % forcing % surfaceWindStressMagnitude % ioinfo % restart .and. output_obj % stream == RESTART) &
.or. &
          (domain % blocklist % forcing % surfaceWindStressMagnitude % ioinfo % sfc .and. output_obj % stream == SFC)) then
         call MPAS_streamAddField(output_obj % io_stream, domain % blocklist % forcing % surfaceWindStressMagnitude, ierr)
      end if

      if ((domain % blocklist % forcing % surfaceMassFlux % ioinfo % output .and. output_obj % stream == OUTPUT) .or. &
          (domain % blocklist % forcing % surfaceMassFlux % ioinfo % restart .and. output_obj % stream == RESTART) .or. &
          (domain % blocklist % forcing % surfaceMassFlux % ioinfo % sfc .and. output_obj % stream == SFC)) then
         call MPAS_streamAddField(output_obj % io_stream, domain % blocklist % forcing % surfaceMassFlux, ierr)
      end if

      if ((domain % blocklist % forcing % seaSurfacePressure % ioinfo % output .and. output_obj % stream == OUTPUT) .or. &
          (domain % blocklist % forcing % seaSurfacePressure % ioinfo % restart .and. output_obj % stream == RESTART) .or. &
          (domain % blocklist % forcing % seaSurfacePressure % ioinfo % sfc .and. output_obj % stream == SFC)) then
         call MPAS_streamAddField(output_obj % io_stream, domain % blocklist % forcing % seaSurfacePressure, ierr)
      end if

      if ((domain % blocklist % forcing % seaIceEnergy % ioinfo % output .and. output_obj % stream == OUTPUT) .or. &
          (domain % blocklist % forcing % seaIceEnergy % ioinfo % restart .and. output_obj % stream == RESTART) .or. &
          (domain % blocklist % forcing % seaIceEnergy % ioinfo % sfc .and. output_obj % stream == SFC)) then
         call MPAS_streamAddField(output_obj % io_stream, domain % blocklist % forcing % seaIceEnergy, ierr)
      end if

      if ((domain % blocklist % forcing % penetrativeTemperatureFlux % ioinfo % output .and. output_obj % stream == OUTPUT) .or. &
          (domain % blocklist % forcing % penetrativeTemperatureFlux % ioinfo % restart .and. output_obj % stream == RESTART) &
.or. &
          (domain % blocklist % forcing % penetrativeTemperatureFlux % ioinfo % sfc .and. output_obj % stream == SFC)) then
         call MPAS_streamAddField(output_obj % io_stream, domain % blocklist % forcing % penetrativeTemperatureFlux, ierr)
      end if

      if ((domain % blocklist % forcing % transmissionCoefficients % ioinfo % output .and. output_obj % stream == OUTPUT) .or. &
          (domain % blocklist % forcing % transmissionCoefficients % ioinfo % restart .and. output_obj % stream == RESTART) .or. &
          (domain % blocklist % forcing % transmissionCoefficients % ioinfo % sfc .and. output_obj % stream == SFC)) then
         call MPAS_streamAddField(output_obj % io_stream, domain % blocklist % forcing % transmissionCoefficients, ierr)
      end if

      if ((domain % blocklist % forcing % windStressZonal % ioinfo % output .and. output_obj % stream == OUTPUT) .or. &
          (domain % blocklist % forcing % windStressZonal % ioinfo % restart .and. output_obj % stream == RESTART) .or. &
          (domain % blocklist % forcing % windStressZonal % ioinfo % sfc .and. output_obj % stream == SFC)) then
         call MPAS_streamAddField(output_obj % io_stream, domain % blocklist % forcing % windStressZonal, ierr)
      end if

      if ((domain % blocklist % forcing % windStressMeridional % ioinfo % output .and. output_obj % stream == OUTPUT) .or. &
          (domain % blocklist % forcing % windStressMeridional % ioinfo % restart .and. output_obj % stream == RESTART) .or. &
          (domain % blocklist % forcing % windStressMeridional % ioinfo % sfc .and. output_obj % stream == SFC)) then
         call MPAS_streamAddField(output_obj % io_stream, domain % blocklist % forcing % windStressMeridional, ierr)
      end if

      if ((domain % blocklist % forcing % latentHeatFlux % ioinfo % output .and. output_obj % stream == OUTPUT) .or. &
          (domain % blocklist % forcing % latentHeatFlux % ioinfo % restart .and. output_obj % stream == RESTART) .or. &
          (domain % blocklist % forcing % latentHeatFlux % ioinfo % sfc .and. output_obj % stream == SFC)) then
         call MPAS_streamAddField(output_obj % io_stream, domain % blocklist % forcing % latentHeatFlux, ierr)
      end if

      if ((domain % blocklist % forcing % sensibleHeatFlux % ioinfo % output .and. output_obj % stream == OUTPUT) .or. &
          (domain % blocklist % forcing % sensibleHeatFlux % ioinfo % restart .and. output_obj % stream == RESTART) .or. &
          (domain % blocklist % forcing % sensibleHeatFlux % ioinfo % sfc .and. output_obj % stream == SFC)) then
         call MPAS_streamAddField(output_obj % io_stream, domain % blocklist % forcing % sensibleHeatFlux, ierr)
      end if

      if ((domain % blocklist % forcing % longWaveHeatFluxUp % ioinfo % output .and. output_obj % stream == OUTPUT) .or. &
          (domain % blocklist % forcing % longWaveHeatFluxUp % ioinfo % restart .and. output_obj % stream == RESTART) .or. &
          (domain % blocklist % forcing % longWaveHeatFluxUp % ioinfo % sfc .and. output_obj % stream == SFC)) then
         call MPAS_streamAddField(output_obj % io_stream, domain % blocklist % forcing % longWaveHeatFluxUp, ierr)
      end if

      if ((domain % blocklist % forcing % longWaveHeatFluxDown % ioinfo % output .and. output_obj % stream == OUTPUT) .or. &
          (domain % blocklist % forcing % longWaveHeatFluxDown % ioinfo % restart .and. output_obj % stream == RESTART) .or. &
          (domain % blocklist % forcing % longWaveHeatFluxDown % ioinfo % sfc .and. output_obj % stream == SFC)) then
         call MPAS_streamAddField(output_obj % io_stream, domain % blocklist % forcing % longWaveHeatFluxDown, ierr)
      end if

      if ((domain % blocklist % forcing % seaIceHeatFlux % ioinfo % output .and. output_obj % stream == OUTPUT) .or. &
          (domain % blocklist % forcing % seaIceHeatFlux % ioinfo % restart .and. output_obj % stream == RESTART) .or. &
          (domain % blocklist % forcing % seaIceHeatFlux % ioinfo % sfc .and. output_obj % stream == SFC)) then
         call MPAS_streamAddField(output_obj % io_stream, domain % blocklist % forcing % seaIceHeatFlux, ierr)
      end if

      if ((domain % blocklist % forcing % shortWaveHeatFlux % ioinfo % output .and. output_obj % stream == OUTPUT) .or. &
          (domain % blocklist % forcing % shortWaveHeatFlux % ioinfo % restart .and. output_obj % stream == RESTART) .or. &
          (domain % blocklist % forcing % shortWaveHeatFlux % ioinfo % sfc .and. output_obj % stream == SFC)) then
         call MPAS_streamAddField(output_obj % io_stream, domain % blocklist % forcing % shortWaveHeatFlux, ierr)
      end if

      if ((domain % blocklist % forcing % evaporationFlux % ioinfo % output .and. output_obj % stream == OUTPUT) .or. &
          (domain % blocklist % forcing % evaporationFlux % ioinfo % restart .and. output_obj % stream == RESTART) .or. &
          (domain % blocklist % forcing % evaporationFlux % ioinfo % sfc .and. output_obj % stream == SFC)) then
         call MPAS_streamAddField(output_obj % io_stream, domain % blocklist % forcing % evaporationFlux, ierr)
      end if

      if ((domain % blocklist % forcing % seaIceSalinityFlux % ioinfo % output .and. output_obj % stream == OUTPUT) .or. &
          (domain % blocklist % forcing % seaIceSalinityFlux % ioinfo % restart .and. output_obj % stream == RESTART) .or. &
          (domain % blocklist % forcing % seaIceSalinityFlux % ioinfo % sfc .and. output_obj % stream == SFC)) then
         call MPAS_streamAddField(output_obj % io_stream, domain % blocklist % forcing % seaIceSalinityFlux, ierr)
      end if

      if ((domain % blocklist % forcing % seaIceFreshWaterFlux % ioinfo % output .and. output_obj % stream == OUTPUT) .or. &
          (domain % blocklist % forcing % seaIceFreshWaterFlux % ioinfo % restart .and. output_obj % stream == RESTART) .or. &
          (domain % blocklist % forcing % seaIceFreshWaterFlux % ioinfo % sfc .and. output_obj % stream == SFC)) then
         call MPAS_streamAddField(output_obj % io_stream, domain % blocklist % forcing % seaIceFreshWaterFlux, ierr)
      end if

      if ((domain % blocklist % forcing % riverRunoffFlux % ioinfo % output .and. output_obj % stream == OUTPUT) .or. &
          (domain % blocklist % forcing % riverRunoffFlux % ioinfo % restart .and. output_obj % stream == RESTART) .or. &
          (domain % blocklist % forcing % riverRunoffFlux % ioinfo % sfc .and. output_obj % stream == SFC)) then
         call MPAS_streamAddField(output_obj % io_stream, domain % blocklist % forcing % riverRunoffFlux, ierr)
      end if

      if ((domain % blocklist % forcing % iceRunoffFlux % ioinfo % output .and. output_obj % stream == OUTPUT) .or. &
          (domain % blocklist % forcing % iceRunoffFlux % ioinfo % restart .and. output_obj % stream == RESTART) .or. &
          (domain % blocklist % forcing % iceRunoffFlux % ioinfo % sfc .and. output_obj % stream == SFC)) then
         call MPAS_streamAddField(output_obj % io_stream, domain % blocklist % forcing % iceRunoffFlux, ierr)
      end if

      if ((domain % blocklist % forcing % rainFlux % ioinfo % output .and. output_obj % stream == OUTPUT) .or. &
          (domain % blocklist % forcing % rainFlux % ioinfo % restart .and. output_obj % stream == RESTART) .or. &
          (domain % blocklist % forcing % rainFlux % ioinfo % sfc .and. output_obj % stream == SFC)) then
         call MPAS_streamAddField(output_obj % io_stream, domain % blocklist % forcing % rainFlux, ierr)
      end if

      if ((domain % blocklist % forcing % snowFlux % ioinfo % output .and. output_obj % stream == OUTPUT) .or. &
          (domain % blocklist % forcing % snowFlux % ioinfo % restart .and. output_obj % stream == RESTART) .or. &
          (domain % blocklist % forcing % snowFlux % ioinfo % sfc .and. output_obj % stream == SFC)) then
         call MPAS_streamAddField(output_obj % io_stream, domain % blocklist % forcing % snowFlux, ierr)
      end if

      if ((domain % blocklist % forcing % iceFraction % ioinfo % output .and. output_obj % stream == OUTPUT) .or. &
          (domain % blocklist % forcing % iceFraction % ioinfo % restart .and. output_obj % stream == RESTART) .or. &
          (domain % blocklist % forcing % iceFraction % ioinfo % sfc .and. output_obj % stream == SFC)) then
         call MPAS_streamAddField(output_obj % io_stream, domain % blocklist % forcing % iceFraction, ierr)
      end if

      if ((domain % blocklist % forcing % prognosticCO2 % ioinfo % output .and. output_obj % stream == OUTPUT) .or. &
          (domain % blocklist % forcing % prognosticCO2 % ioinfo % restart .and. output_obj % stream == RESTART) .or. &
          (domain % blocklist % forcing % prognosticCO2 % ioinfo % sfc .and. output_obj % stream == SFC)) then
         call MPAS_streamAddField(output_obj % io_stream, domain % blocklist % forcing % prognosticCO2, ierr)
      end if

      if ((domain % blocklist % forcing % diagnosticCO2 % ioinfo % output .and. output_obj % stream == OUTPUT) .or. &
          (domain % blocklist % forcing % diagnosticCO2 % ioinfo % restart .and. output_obj % stream == RESTART) .or. &
          (domain % blocklist % forcing % diagnosticCO2 % ioinfo % sfc .and. output_obj % stream == SFC)) then
         call MPAS_streamAddField(output_obj % io_stream, domain % blocklist % forcing % diagnosticCO2, ierr)
      end if

      if ((domain % blocklist % forcing % squaredWindSpeed10Meter % ioinfo % output .and. output_obj % stream == OUTPUT) .or. &
          (domain % blocklist % forcing % squaredWindSpeed10Meter % ioinfo % restart .and. output_obj % stream == RESTART) .or. &
          (domain % blocklist % forcing % squaredWindSpeed10Meter % ioinfo % sfc .and. output_obj % stream == SFC)) then
         call MPAS_streamAddField(output_obj % io_stream, domain % blocklist % forcing % squaredWindSpeed10Meter, ierr)
      end if

      if ((domain % blocklist % forcing % nAccumulatedCoupled % ioinfo % output .and. output_obj % stream == OUTPUT) .or. &
          (domain % blocklist % forcing % nAccumulatedCoupled % ioinfo % restart .and. output_obj % stream == RESTART) .or. &
          (domain % blocklist % forcing % nAccumulatedCoupled % ioinfo % sfc .and. output_obj % stream == SFC)) then
         call MPAS_streamAddField(output_obj % io_stream, domain % blocklist % forcing % nAccumulatedCoupled, ierr)
      end if

      if ((domain % blocklist % forcing % CO2Flux % ioinfo % output .and. output_obj % stream == OUTPUT) .or. &
          (domain % blocklist % forcing % CO2Flux % ioinfo % restart .and. output_obj % stream == RESTART) .or. &
          (domain % blocklist % forcing % CO2Flux % ioinfo % sfc .and. output_obj % stream == SFC)) then
         call MPAS_streamAddField(output_obj % io_stream, domain % blocklist % forcing % CO2Flux, ierr)
      end if

      if ((domain % blocklist % forcing % DMSFlux % ioinfo % output .and. output_obj % stream == OUTPUT) .or. &
          (domain % blocklist % forcing % DMSFlux % ioinfo % restart .and. output_obj % stream == RESTART) .or. &
          (domain % blocklist % forcing % DMSFlux % ioinfo % sfc .and. output_obj % stream == SFC)) then
         call MPAS_streamAddField(output_obj % io_stream, domain % blocklist % forcing % DMSFlux, ierr)
      end if

      if ((domain % blocklist % scratch % vorticityGradientTangentialComponent % ioinfo % output .and. output_obj % stream == &
OUTPUT) .or. &
          (domain % blocklist % scratch % vorticityGradientTangentialComponent % ioinfo % restart .and. output_obj % stream == &
RESTART) .or. &
          (domain % blocklist % scratch % vorticityGradientTangentialComponent % ioinfo % sfc .and. output_obj % stream == SFC)) &
then
         call MPAS_streamAddField(output_obj % io_stream, domain % blocklist % scratch % vorticityGradientTangentialComponent, &
ierr)
      end if

      if ((domain % blocklist % scratch % vorticityGradientNormalComponent % ioinfo % output .and. output_obj % stream == OUTPUT) &
.or. &
          (domain % blocklist % scratch % vorticityGradientNormalComponent % ioinfo % restart .and. output_obj % stream == &
RESTART) .or. &
          (domain % blocklist % scratch % vorticityGradientNormalComponent % ioinfo % sfc .and. output_obj % stream == SFC)) then
         call MPAS_streamAddField(output_obj % io_stream, domain % blocklist % scratch % vorticityGradientNormalComponent, ierr)
      end if

      if ((domain % blocklist % scratch % normalizedRelativeVorticityVertex % ioinfo % output .and. output_obj % stream == OUTPUT) &
.or. &
          (domain % blocklist % scratch % normalizedRelativeVorticityVertex % ioinfo % restart .and. output_obj % stream == &
RESTART) .or. &
          (domain % blocklist % scratch % normalizedRelativeVorticityVertex % ioinfo % sfc .and. output_obj % stream == SFC)) then
         call MPAS_streamAddField(output_obj % io_stream, domain % blocklist % scratch % normalizedRelativeVorticityVertex, ierr)
      end if

      if ((domain % blocklist % scratch % normalizedPlanetaryVorticityVertex % ioinfo % output .and. output_obj % stream == &
OUTPUT) .or. &
          (domain % blocklist % scratch % normalizedPlanetaryVorticityVertex % ioinfo % restart .and. output_obj % stream == &
RESTART) .or. &
          (domain % blocklist % scratch % normalizedPlanetaryVorticityVertex % ioinfo % sfc .and. output_obj % stream == SFC)) then
         call MPAS_streamAddField(output_obj % io_stream, domain % blocklist % scratch % normalizedPlanetaryVorticityVertex, ierr)
      end if

      if ((domain % blocklist % scratch % kineticEnergyVertex % ioinfo % output .and. output_obj % stream == OUTPUT) .or. &
          (domain % blocklist % scratch % kineticEnergyVertex % ioinfo % restart .and. output_obj % stream == RESTART) .or. &
          (domain % blocklist % scratch % kineticEnergyVertex % ioinfo % sfc .and. output_obj % stream == SFC)) then
         call MPAS_streamAddField(output_obj % io_stream, domain % blocklist % scratch % kineticEnergyVertex, ierr)
      end if

      if ((domain % blocklist % scratch % kineticEnergyVertexOnCells % ioinfo % output .and. output_obj % stream == OUTPUT) .or. &
          (domain % blocklist % scratch % kineticEnergyVertexOnCells % ioinfo % restart .and. output_obj % stream == RESTART) &
.or. &
          (domain % blocklist % scratch % kineticEnergyVertexOnCells % ioinfo % sfc .and. output_obj % stream == SFC)) then
         call MPAS_streamAddField(output_obj % io_stream, domain % blocklist % scratch % kineticEnergyVertexOnCells, ierr)
      end if

      if ((domain % blocklist % scratch % densitySurfaceDisplaced % ioinfo % output .and. output_obj % stream == OUTPUT) .or. &
          (domain % blocklist % scratch % densitySurfaceDisplaced % ioinfo % restart .and. output_obj % stream == RESTART) .or. &
          (domain % blocklist % scratch % densitySurfaceDisplaced % ioinfo % sfc .and. output_obj % stream == SFC)) then
         call MPAS_streamAddField(output_obj % io_stream, domain % blocklist % scratch % densitySurfaceDisplaced, ierr)
      end if

      if ((domain % blocklist % scratch % thermalExpansionCoeff % ioinfo % output .and. output_obj % stream == OUTPUT) .or. &
          (domain % blocklist % scratch % thermalExpansionCoeff % ioinfo % restart .and. output_obj % stream == RESTART) .or. &
          (domain % blocklist % scratch % thermalExpansionCoeff % ioinfo % sfc .and. output_obj % stream == SFC)) then
         call MPAS_streamAddField(output_obj % io_stream, domain % blocklist % scratch % thermalExpansionCoeff, ierr)
      end if

      if ((domain % blocklist % scratch % salineContractionCoeff % ioinfo % output .and. output_obj % stream == OUTPUT) .or. &
          (domain % blocklist % scratch % salineContractionCoeff % ioinfo % restart .and. output_obj % stream == RESTART) .or. &
          (domain % blocklist % scratch % salineContractionCoeff % ioinfo % sfc .and. output_obj % stream == SFC)) then
         call MPAS_streamAddField(output_obj % io_stream, domain % blocklist % scratch % salineContractionCoeff, ierr)
      end if

      if ((domain % blocklist % scratch % normalVelocityTest % ioinfo % output .and. output_obj % stream == OUTPUT) .or. &
          (domain % blocklist % scratch % normalVelocityTest % ioinfo % restart .and. output_obj % stream == RESTART) .or. &
          (domain % blocklist % scratch % normalVelocityTest % ioinfo % sfc .and. output_obj % stream == SFC)) then
         call MPAS_streamAddField(output_obj % io_stream, domain % blocklist % scratch % normalVelocityTest, ierr)
      end if

      if ((domain % blocklist % scratch % tangentialVelocityTest % ioinfo % output .and. output_obj % stream == OUTPUT) .or. &
          (domain % blocklist % scratch % tangentialVelocityTest % ioinfo % restart .and. output_obj % stream == RESTART) .or. &
          (domain % blocklist % scratch % tangentialVelocityTest % ioinfo % sfc .and. output_obj % stream == SFC)) then
         call MPAS_streamAddField(output_obj % io_stream, domain % blocklist % scratch % tangentialVelocityTest, ierr)
      end if

      if ((domain % blocklist % scratch % strainRateR3Cell % ioinfo % output .and. output_obj % stream == OUTPUT) .or. &
          (domain % blocklist % scratch % strainRateR3Cell % ioinfo % restart .and. output_obj % stream == RESTART) .or. &
          (domain % blocklist % scratch % strainRateR3Cell % ioinfo % sfc .and. output_obj % stream == SFC)) then
         call MPAS_streamAddField(output_obj % io_stream, domain % blocklist % scratch % strainRateR3Cell, ierr)
      end if

      if ((domain % blocklist % scratch % strainRateR3CellSolution % ioinfo % output .and. output_obj % stream == OUTPUT) .or. &
          (domain % blocklist % scratch % strainRateR3CellSolution % ioinfo % restart .and. output_obj % stream == RESTART) .or. &
          (domain % blocklist % scratch % strainRateR3CellSolution % ioinfo % sfc .and. output_obj % stream == SFC)) then
         call MPAS_streamAddField(output_obj % io_stream, domain % blocklist % scratch % strainRateR3CellSolution, ierr)
      end if

      if ((domain % blocklist % scratch % strainRateR3Edge % ioinfo % output .and. output_obj % stream == OUTPUT) .or. &
          (domain % blocklist % scratch % strainRateR3Edge % ioinfo % restart .and. output_obj % stream == RESTART) .or. &
          (domain % blocklist % scratch % strainRateR3Edge % ioinfo % sfc .and. output_obj % stream == SFC)) then
         call MPAS_streamAddField(output_obj % io_stream, domain % blocklist % scratch % strainRateR3Edge, ierr)
      end if

      if ((domain % blocklist % scratch % strainRateLonLatRCell % ioinfo % output .and. output_obj % stream == OUTPUT) .or. &
          (domain % blocklist % scratch % strainRateLonLatRCell % ioinfo % restart .and. output_obj % stream == RESTART) .or. &
          (domain % blocklist % scratch % strainRateLonLatRCell % ioinfo % sfc .and. output_obj % stream == SFC)) then
         call MPAS_streamAddField(output_obj % io_stream, domain % blocklist % scratch % strainRateLonLatRCell, ierr)
      end if

      if ((domain % blocklist % scratch % strainRateLonLatRCellSolution % ioinfo % output .and. output_obj % stream == OUTPUT) &
.or. &
          (domain % blocklist % scratch % strainRateLonLatRCellSolution % ioinfo % restart .and. output_obj % stream == RESTART) &
.or. &
          (domain % blocklist % scratch % strainRateLonLatRCellSolution % ioinfo % sfc .and. output_obj % stream == SFC)) then
         call MPAS_streamAddField(output_obj % io_stream, domain % blocklist % scratch % strainRateLonLatRCellSolution, ierr)
      end if

      if ((domain % blocklist % scratch % strainRateLonLatREdge % ioinfo % output .and. output_obj % stream == OUTPUT) .or. &
          (domain % blocklist % scratch % strainRateLonLatREdge % ioinfo % restart .and. output_obj % stream == RESTART) .or. &
          (domain % blocklist % scratch % strainRateLonLatREdge % ioinfo % sfc .and. output_obj % stream == SFC)) then
         call MPAS_streamAddField(output_obj % io_stream, domain % blocklist % scratch % strainRateLonLatREdge, ierr)
      end if

      if ((domain % blocklist % scratch % divTensorR3Cell % ioinfo % output .and. output_obj % stream == OUTPUT) .or. &
          (domain % blocklist % scratch % divTensorR3Cell % ioinfo % restart .and. output_obj % stream == RESTART) .or. &
          (domain % blocklist % scratch % divTensorR3Cell % ioinfo % sfc .and. output_obj % stream == SFC)) then
         call MPAS_streamAddField(output_obj % io_stream, domain % blocklist % scratch % divTensorR3Cell, ierr)
      end if

      if ((domain % blocklist % scratch % divTensorR3CellSolution % ioinfo % output .and. output_obj % stream == OUTPUT) .or. &
          (domain % blocklist % scratch % divTensorR3CellSolution % ioinfo % restart .and. output_obj % stream == RESTART) .or. &
          (domain % blocklist % scratch % divTensorR3CellSolution % ioinfo % sfc .and. output_obj % stream == SFC)) then
         call MPAS_streamAddField(output_obj % io_stream, domain % blocklist % scratch % divTensorR3CellSolution, ierr)
      end if

      if ((domain % blocklist % scratch % divTensorLonLatRCell % ioinfo % output .and. output_obj % stream == OUTPUT) .or. &
          (domain % blocklist % scratch % divTensorLonLatRCell % ioinfo % restart .and. output_obj % stream == RESTART) .or. &
          (domain % blocklist % scratch % divTensorLonLatRCell % ioinfo % sfc .and. output_obj % stream == SFC)) then
         call MPAS_streamAddField(output_obj % io_stream, domain % blocklist % scratch % divTensorLonLatRCell, ierr)
      end if

      if ((domain % blocklist % scratch % divTensorLonLatRCellSolution % ioinfo % output .and. output_obj % stream == OUTPUT) &
.or. &
          (domain % blocklist % scratch % divTensorLonLatRCellSolution % ioinfo % restart .and. output_obj % stream == RESTART) &
.or. &
          (domain % blocklist % scratch % divTensorLonLatRCellSolution % ioinfo % sfc .and. output_obj % stream == SFC)) then
         call MPAS_streamAddField(output_obj % io_stream, domain % blocklist % scratch % divTensorLonLatRCellSolution, ierr)
      end if

      if ((domain % blocklist % scratch % outerProductEdge % ioinfo % output .and. output_obj % stream == OUTPUT) .or. &
          (domain % blocklist % scratch % outerProductEdge % ioinfo % restart .and. output_obj % stream == RESTART) .or. &
          (domain % blocklist % scratch % outerProductEdge % ioinfo % sfc .and. output_obj % stream == SFC)) then
         call MPAS_streamAddField(output_obj % io_stream, domain % blocklist % scratch % outerProductEdge, ierr)
      end if

      if ((domain % blocklist % scratch % normalVectorEdge % ioinfo % output .and. output_obj % stream == OUTPUT) .or. &
          (domain % blocklist % scratch % normalVectorEdge % ioinfo % restart .and. output_obj % stream == RESTART) .or. &
          (domain % blocklist % scratch % normalVectorEdge % ioinfo % sfc .and. output_obj % stream == SFC)) then
         call MPAS_streamAddField(output_obj % io_stream, domain % blocklist % scratch % normalVectorEdge, ierr)
      end if

      if ((domain % blocklist % scratch % tangentialVectorEdge % ioinfo % output .and. output_obj % stream == OUTPUT) .or. &
          (domain % blocklist % scratch % tangentialVectorEdge % ioinfo % restart .and. output_obj % stream == RESTART) .or. &
          (domain % blocklist % scratch % tangentialVectorEdge % ioinfo % sfc .and. output_obj % stream == SFC)) then
         call MPAS_streamAddField(output_obj % io_stream, domain % blocklist % scratch % tangentialVectorEdge, ierr)
      end if

      if ((domain % blocklist % scratch % windStressFull % ioinfo % output .and. output_obj % stream == OUTPUT) .or. &
          (domain % blocklist % scratch % windStressFull % ioinfo % restart .and. output_obj % stream == RESTART) .or. &
          (domain % blocklist % scratch % windStressFull % ioinfo % sfc .and. output_obj % stream == SFC)) then
         call MPAS_streamAddField(output_obj % io_stream, domain % blocklist % scratch % windStressFull, ierr)
      end if

      if ((domain % blocklist % scratch % windStressX % ioinfo % output .and. output_obj % stream == OUTPUT) .or. &
          (domain % blocklist % scratch % windStressX % ioinfo % restart .and. output_obj % stream == RESTART) .or. &
          (domain % blocklist % scratch % windStressX % ioinfo % sfc .and. output_obj % stream == SFC)) then
         call MPAS_streamAddField(output_obj % io_stream, domain % blocklist % scratch % windStressX, ierr)
      end if

      if ((domain % blocklist % scratch % windStressY % ioinfo % output .and. output_obj % stream == OUTPUT) .or. &
          (domain % blocklist % scratch % windStressY % ioinfo % restart .and. output_obj % stream == RESTART) .or. &
          (domain % blocklist % scratch % windStressY % ioinfo % sfc .and. output_obj % stream == SFC)) then
         call MPAS_streamAddField(output_obj % io_stream, domain % blocklist % scratch % windStressY, ierr)
      end if

      if ((domain % blocklist % scratch % windStressZ % ioinfo % output .and. output_obj % stream == OUTPUT) .or. &
          (domain % blocklist % scratch % windStressZ % ioinfo % restart .and. output_obj % stream == RESTART) .or. &
          (domain % blocklist % scratch % windStressZ % ioinfo % sfc .and. output_obj % stream == SFC)) then
         call MPAS_streamAddField(output_obj % io_stream, domain % blocklist % scratch % windStressZ, ierr)
      end if

      if ((domain % blocklist % scratch % windStressZonal % ioinfo % output .and. output_obj % stream == OUTPUT) .or. &
          (domain % blocklist % scratch % windStressZonal % ioinfo % restart .and. output_obj % stream == RESTART) .or. &
          (domain % blocklist % scratch % windStressZonal % ioinfo % sfc .and. output_obj % stream == SFC)) then
         call MPAS_streamAddField(output_obj % io_stream, domain % blocklist % scratch % windStressZonal, ierr)
      end if

      if ((domain % blocklist % scratch % windStressMeridional % ioinfo % output .and. output_obj % stream == OUTPUT) .or. &
          (domain % blocklist % scratch % windStressMeridional % ioinfo % restart .and. output_obj % stream == RESTART) .or. &
          (domain % blocklist % scratch % windStressMeridional % ioinfo % sfc .and. output_obj % stream == SFC)) then
         call MPAS_streamAddField(output_obj % io_stream, domain % blocklist % scratch % windStressMeridional, ierr)
      end if


      if (mesh % on_a_sphere) then
         call MPAS_writeStreamAtt(output_obj % io_stream, 'on_a_sphere', 'YES             ', nferr)
      else
         call MPAS_writeStreamAtt(output_obj % io_stream, 'on_a_sphere', 'NO              ', nferr)
      end if
      call MPAS_writeStreamAtt(output_obj % io_stream, 'sphere_radius', mesh % sphere_radius, nferr)
      call MPAS_writeStreamAtt(output_obj % io_stream, 'model_name', domain % modelName, nferr)
      call MPAS_writeStreamAtt(output_obj % io_stream, 'core_name', domain % coreName, nferr)
      call MPAS_writeStreamAtt(output_obj % io_stream, 'model_version', domain % modelVersion, nferr)
      call MPAS_writeStreamAtt(output_obj % io_stream, 'history', runCmd, nferr)
      call MPAS_writeStreamAtt(output_obj % io_stream, 'source', 'MPAS', nferr)
      call MPAS_writeStreamAtt(output_obj % io_stream, 'Conventions', 'MPAS', nferr)

      if (config_do_restart) then
         call MPAS_writeStreamAtt(output_obj % io_stream, 'config_do_restart', 'T', ierr)
      else
         call MPAS_writeStreamAtt(output_obj % io_stream, 'config_do_restart', 'F', ierr)
      end if
      call MPAS_writeStreamAtt(output_obj % io_stream, 'config_start_time', config_start_time, ierr)
      call MPAS_writeStreamAtt(output_obj % io_stream, 'config_stop_time', config_stop_time, ierr)
      call MPAS_writeStreamAtt(output_obj % io_stream, 'config_run_duration', config_run_duration, ierr)
      call MPAS_writeStreamAtt(output_obj % io_stream, 'config_calendar_type', config_calendar_type, ierr)
      call MPAS_writeStreamAtt(output_obj % io_stream, 'config_input_name', config_input_name, ierr)
      call MPAS_writeStreamAtt(output_obj % io_stream, 'config_output_name', config_output_name, ierr)
      call MPAS_writeStreamAtt(output_obj % io_stream, 'config_restart_name', config_restart_name, ierr)
      call MPAS_writeStreamAtt(output_obj % io_stream, 'config_restart_timestamp_name', config_restart_timestamp_name, ierr)
      call MPAS_writeStreamAtt(output_obj % io_stream, 'config_restart_interval', config_restart_interval, ierr)
      call MPAS_writeStreamAtt(output_obj % io_stream, 'config_output_interval', config_output_interval, ierr)
      call MPAS_writeStreamAtt(output_obj % io_stream, 'config_stats_interval', config_stats_interval, ierr)
      if (config_write_stats_on_startup) then
         call MPAS_writeStreamAtt(output_obj % io_stream, 'config_write_stats_on_startup', 'T', ierr)
      else
         call MPAS_writeStreamAtt(output_obj % io_stream, 'config_write_stats_on_startup', 'F', ierr)
      end if
      if (config_write_output_on_startup) then
         call MPAS_writeStreamAtt(output_obj % io_stream, 'config_write_output_on_startup', 'T', ierr)
      else
         call MPAS_writeStreamAtt(output_obj % io_stream, 'config_write_output_on_startup', 'F', ierr)
      end if
      call MPAS_writeStreamAtt(output_obj % io_stream, 'config_frames_per_outfile', config_frames_per_outfile, ierr)
      call MPAS_writeStreamAtt(output_obj % io_stream, 'config_pio_num_iotasks', config_pio_num_iotasks, ierr)
      call MPAS_writeStreamAtt(output_obj % io_stream, 'config_pio_stride', config_pio_stride, ierr)
      call MPAS_writeStreamAtt(output_obj % io_stream, 'config_dt', config_dt, ierr)
      call MPAS_writeStreamAtt(output_obj % io_stream, 'config_time_integrator', config_time_integrator, ierr)
      call MPAS_writeStreamAtt(output_obj % io_stream, 'config_vert_coord_movement', config_vert_coord_movement, ierr)
      if (config_use_min_max_thickness) then
         call MPAS_writeStreamAtt(output_obj % io_stream, 'config_use_min_max_thickness', 'T', ierr)
      else
         call MPAS_writeStreamAtt(output_obj % io_stream, 'config_use_min_max_thickness', 'F', ierr)
      end if
      call MPAS_writeStreamAtt(output_obj % io_stream, 'config_min_thickness', config_min_thickness, ierr)
      call MPAS_writeStreamAtt(output_obj % io_stream, 'config_max_thickness_factor', config_max_thickness_factor, ierr)
      if (config_set_restingThickness_to_IC) then
         call MPAS_writeStreamAtt(output_obj % io_stream, 'config_set_restingThickness_to_IC', 'T', ierr)
      else
         call MPAS_writeStreamAtt(output_obj % io_stream, 'config_set_restingThickness_to_IC', 'F', ierr)
      end if
      if (config_dzdk_positive) then
         call MPAS_writeStreamAtt(output_obj % io_stream, 'config_dzdk_positive', 'T', ierr)
      else
         call MPAS_writeStreamAtt(output_obj % io_stream, 'config_dzdk_positive', 'F', ierr)
      end if
      if (config_use_freq_filtered_thickness) then
         call MPAS_writeStreamAtt(output_obj % io_stream, 'config_use_freq_filtered_thickness', 'T', ierr)
      else
         call MPAS_writeStreamAtt(output_obj % io_stream, 'config_use_freq_filtered_thickness', 'F', ierr)
      end if
      call MPAS_writeStreamAtt(output_obj % io_stream, 'config_thickness_filter_timescale', config_thickness_filter_timescale, &
ierr)
      if (config_use_highFreqThick_restore) then
         call MPAS_writeStreamAtt(output_obj % io_stream, 'config_use_highFreqThick_restore', 'T', ierr)
      else
         call MPAS_writeStreamAtt(output_obj % io_stream, 'config_use_highFreqThick_restore', 'F', ierr)
      end if
      call MPAS_writeStreamAtt(output_obj % io_stream, 'config_highFreqThick_restore_time', config_highFreqThick_restore_time, &
ierr)
      if (config_use_highFreqThick_del2) then
         call MPAS_writeStreamAtt(output_obj % io_stream, 'config_use_highFreqThick_del2', 'T', ierr)
      else
         call MPAS_writeStreamAtt(output_obj % io_stream, 'config_use_highFreqThick_del2', 'F', ierr)
      end if
      call MPAS_writeStreamAtt(output_obj % io_stream, 'config_highFreqThick_del2', config_highFreqThick_del2, ierr)
      if (config_alter_ICs_for_pbcs) then
         call MPAS_writeStreamAtt(output_obj % io_stream, 'config_alter_ICs_for_pbcs', 'T', ierr)
      else
         call MPAS_writeStreamAtt(output_obj % io_stream, 'config_alter_ICs_for_pbcs', 'F', ierr)
      end if
      call MPAS_writeStreamAtt(output_obj % io_stream, 'config_pbc_alteration_type', config_pbc_alteration_type, ierr)
      call MPAS_writeStreamAtt(output_obj % io_stream, 'config_min_pbc_fraction', config_min_pbc_fraction, ierr)
      if (config_check_ssh_consistency) then
         call MPAS_writeStreamAtt(output_obj % io_stream, 'config_check_ssh_consistency', 'T', ierr)
      else
         call MPAS_writeStreamAtt(output_obj % io_stream, 'config_check_ssh_consistency', 'F', ierr)
      end if
      call MPAS_writeStreamAtt(output_obj % io_stream, 'config_num_halos', config_num_halos, ierr)
      call MPAS_writeStreamAtt(output_obj % io_stream, 'config_block_decomp_file_prefix', config_block_decomp_file_prefix, ierr)
      call MPAS_writeStreamAtt(output_obj % io_stream, 'config_number_of_blocks', config_number_of_blocks, ierr)
      if (config_explicit_proc_decomp) then
         call MPAS_writeStreamAtt(output_obj % io_stream, 'config_explicit_proc_decomp', 'T', ierr)
      else
         call MPAS_writeStreamAtt(output_obj % io_stream, 'config_explicit_proc_decomp', 'F', ierr)
      end if
      call MPAS_writeStreamAtt(output_obj % io_stream, 'config_proc_decomp_file_prefix', config_proc_decomp_file_prefix, ierr)
      if (config_hmix_ScaleWithMesh) then
         call MPAS_writeStreamAtt(output_obj % io_stream, 'config_hmix_ScaleWithMesh', 'T', ierr)
      else
         call MPAS_writeStreamAtt(output_obj % io_stream, 'config_hmix_ScaleWithMesh', 'F', ierr)
      end if
      call MPAS_writeStreamAtt(output_obj % io_stream, 'config_maxMeshDensity', config_maxMeshDensity, ierr)
      call MPAS_writeStreamAtt(output_obj % io_stream, 'config_apvm_scale_factor', config_apvm_scale_factor, ierr)
      if (config_use_mom_del2) then
         call MPAS_writeStreamAtt(output_obj % io_stream, 'config_use_mom_del2', 'T', ierr)
      else
         call MPAS_writeStreamAtt(output_obj % io_stream, 'config_use_mom_del2', 'F', ierr)
      end if
      if (config_use_tracer_del2) then
         call MPAS_writeStreamAtt(output_obj % io_stream, 'config_use_tracer_del2', 'T', ierr)
      else
         call MPAS_writeStreamAtt(output_obj % io_stream, 'config_use_tracer_del2', 'F', ierr)
      end if
      call MPAS_writeStreamAtt(output_obj % io_stream, 'config_mom_del2', config_mom_del2, ierr)
      call MPAS_writeStreamAtt(output_obj % io_stream, 'config_tracer_del2', config_tracer_del2, ierr)
      if (config_use_mom_del4) then
         call MPAS_writeStreamAtt(output_obj % io_stream, 'config_use_mom_del4', 'T', ierr)
      else
         call MPAS_writeStreamAtt(output_obj % io_stream, 'config_use_mom_del4', 'F', ierr)
      end if
      if (config_use_tracer_del4) then
         call MPAS_writeStreamAtt(output_obj % io_stream, 'config_use_tracer_del4', 'T', ierr)
      else
         call MPAS_writeStreamAtt(output_obj % io_stream, 'config_use_tracer_del4', 'F', ierr)
      end if
      call MPAS_writeStreamAtt(output_obj % io_stream, 'config_mom_del4', config_mom_del4, ierr)
      call MPAS_writeStreamAtt(output_obj % io_stream, 'config_tracer_del4', config_tracer_del4, ierr)
      if (config_use_Leith_del2) then
         call MPAS_writeStreamAtt(output_obj % io_stream, 'config_use_Leith_del2', 'T', ierr)
      else
         call MPAS_writeStreamAtt(output_obj % io_stream, 'config_use_Leith_del2', 'F', ierr)
      end if
      call MPAS_writeStreamAtt(output_obj % io_stream, 'config_Leith_parameter', config_Leith_parameter, ierr)
      call MPAS_writeStreamAtt(output_obj % io_stream, 'config_Leith_dx', config_Leith_dx, ierr)
      call MPAS_writeStreamAtt(output_obj % io_stream, 'config_Leith_visc2_max', config_Leith_visc2_max, ierr)
      call MPAS_writeStreamAtt(output_obj % io_stream, 'config_h_kappa', config_h_kappa, ierr)
      call MPAS_writeStreamAtt(output_obj % io_stream, 'config_h_kappa_q', config_h_kappa_q, ierr)
      if (config_use_mom_del2_tensor) then
         call MPAS_writeStreamAtt(output_obj % io_stream, 'config_use_mom_del2_tensor', 'T', ierr)
      else
         call MPAS_writeStreamAtt(output_obj % io_stream, 'config_use_mom_del2_tensor', 'F', ierr)
      end if
      call MPAS_writeStreamAtt(output_obj % io_stream, 'config_mom_del2_tensor', config_mom_del2_tensor, ierr)
      if (config_use_mom_del4_tensor) then
         call MPAS_writeStreamAtt(output_obj % io_stream, 'config_use_mom_del4_tensor', 'T', ierr)
      else
         call MPAS_writeStreamAtt(output_obj % io_stream, 'config_use_mom_del4_tensor', 'F', ierr)
      end if
      call MPAS_writeStreamAtt(output_obj % io_stream, 'config_mom_del4_tensor', config_mom_del4_tensor, ierr)
      if (config_Rayleigh_friction) then
         call MPAS_writeStreamAtt(output_obj % io_stream, 'config_Rayleigh_friction', 'T', ierr)
      else
         call MPAS_writeStreamAtt(output_obj % io_stream, 'config_Rayleigh_friction', 'F', ierr)
      end if
      call MPAS_writeStreamAtt(output_obj % io_stream, 'config_Rayleigh_damping_coeff', config_Rayleigh_damping_coeff, ierr)
      call MPAS_writeStreamAtt(output_obj % io_stream, 'config_convective_visc', config_convective_visc, ierr)
      call MPAS_writeStreamAtt(output_obj % io_stream, 'config_convective_diff', config_convective_diff, ierr)
      if (config_use_const_visc) then
         call MPAS_writeStreamAtt(output_obj % io_stream, 'config_use_const_visc', 'T', ierr)
      else
         call MPAS_writeStreamAtt(output_obj % io_stream, 'config_use_const_visc', 'F', ierr)
      end if
      if (config_use_const_diff) then
         call MPAS_writeStreamAtt(output_obj % io_stream, 'config_use_const_diff', 'T', ierr)
      else
         call MPAS_writeStreamAtt(output_obj % io_stream, 'config_use_const_diff', 'F', ierr)
      end if
      call MPAS_writeStreamAtt(output_obj % io_stream, 'config_vert_visc', config_vert_visc, ierr)
      call MPAS_writeStreamAtt(output_obj % io_stream, 'config_vert_diff', config_vert_diff, ierr)
      if (config_use_rich_visc) then
         call MPAS_writeStreamAtt(output_obj % io_stream, 'config_use_rich_visc', 'T', ierr)
      else
         call MPAS_writeStreamAtt(output_obj % io_stream, 'config_use_rich_visc', 'F', ierr)
      end if
      if (config_use_rich_diff) then
         call MPAS_writeStreamAtt(output_obj % io_stream, 'config_use_rich_diff', 'T', ierr)
      else
         call MPAS_writeStreamAtt(output_obj % io_stream, 'config_use_rich_diff', 'F', ierr)
      end if
      call MPAS_writeStreamAtt(output_obj % io_stream, 'config_bkrd_vert_visc', config_bkrd_vert_visc, ierr)
      call MPAS_writeStreamAtt(output_obj % io_stream, 'config_bkrd_vert_diff', config_bkrd_vert_diff, ierr)
      call MPAS_writeStreamAtt(output_obj % io_stream, 'config_rich_mix', config_rich_mix, ierr)
      if (config_use_tanh_visc) then
         call MPAS_writeStreamAtt(output_obj % io_stream, 'config_use_tanh_visc', 'T', ierr)
      else
         call MPAS_writeStreamAtt(output_obj % io_stream, 'config_use_tanh_visc', 'F', ierr)
      end if
      if (config_use_tanh_diff) then
         call MPAS_writeStreamAtt(output_obj % io_stream, 'config_use_tanh_diff', 'T', ierr)
      else
         call MPAS_writeStreamAtt(output_obj % io_stream, 'config_use_tanh_diff', 'F', ierr)
      end if
      call MPAS_writeStreamAtt(output_obj % io_stream, 'config_max_visc_tanh', config_max_visc_tanh, ierr)
      call MPAS_writeStreamAtt(output_obj % io_stream, 'config_min_visc_tanh', config_min_visc_tanh, ierr)
      call MPAS_writeStreamAtt(output_obj % io_stream, 'config_max_diff_tanh', config_max_diff_tanh, ierr)
      call MPAS_writeStreamAtt(output_obj % io_stream, 'config_min_diff_tanh', config_min_diff_tanh, ierr)
      call MPAS_writeStreamAtt(output_obj % io_stream, 'config_zMid_tanh', config_zMid_tanh, ierr)
      call MPAS_writeStreamAtt(output_obj % io_stream, 'config_zWidth_tanh', config_zWidth_tanh, ierr)
      if (config_use_cvmix) then
         call MPAS_writeStreamAtt(output_obj % io_stream, 'config_use_cvmix', 'T', ierr)
      else
         call MPAS_writeStreamAtt(output_obj % io_stream, 'config_use_cvmix', 'F', ierr)
      end if
      call MPAS_writeStreamAtt(output_obj % io_stream, 'config_cvmix_prandtl_number', config_cvmix_prandtl_number, ierr)
      if (config_use_cvmix_background) then
         call MPAS_writeStreamAtt(output_obj % io_stream, 'config_use_cvmix_background', 'T', ierr)
      else
         call MPAS_writeStreamAtt(output_obj % io_stream, 'config_use_cvmix_background', 'F', ierr)
      end if
      call MPAS_writeStreamAtt(output_obj % io_stream, 'config_cvmix_background_diffusion', config_cvmix_background_diffusion, &
ierr)
      call MPAS_writeStreamAtt(output_obj % io_stream, 'config_cvmix_background_viscosity', config_cvmix_background_viscosity, &
ierr)
      if (config_use_cvmix_convection) then
         call MPAS_writeStreamAtt(output_obj % io_stream, 'config_use_cvmix_convection', 'T', ierr)
      else
         call MPAS_writeStreamAtt(output_obj % io_stream, 'config_use_cvmix_convection', 'F', ierr)
      end if
      call MPAS_writeStreamAtt(output_obj % io_stream, 'config_cvmix_convective_diffusion', config_cvmix_convective_diffusion, &
ierr)
      call MPAS_writeStreamAtt(output_obj % io_stream, 'config_cvmix_convective_viscosity', config_cvmix_convective_viscosity, &
ierr)
      if (config_use_cvmix_kpp) then
         call MPAS_writeStreamAtt(output_obj % io_stream, 'config_use_cvmix_kpp', 'T', ierr)
      else
         call MPAS_writeStreamAtt(output_obj % io_stream, 'config_use_cvmix_kpp', 'F', ierr)
      end if
      call MPAS_writeStreamAtt(output_obj % io_stream, 'config_cvmix_kpp_criticalBulkRichardsonNumber', &
config_cvmix_kpp_criticalBulkRichardsonNumber, ierr)
      call MPAS_writeStreamAtt(output_obj % io_stream, 'config_cvmix_kpp_interpolationOMLType', &
config_cvmix_kpp_interpolationOMLType, ierr)
      call MPAS_writeStreamAtt(output_obj % io_stream, 'config_forcing_type', config_forcing_type, ierr)
      call MPAS_writeStreamAtt(output_obj % io_stream, 'config_restoreT_timescale', config_restoreT_timescale, ierr)
      call MPAS_writeStreamAtt(output_obj % io_stream, 'config_restoreS_timescale', config_restoreS_timescale, ierr)
      call MPAS_writeStreamAtt(output_obj % io_stream, 'config_restoreT_lengthscale', config_restoreT_lengthscale, ierr)
      call MPAS_writeStreamAtt(output_obj % io_stream, 'config_restoreS_lengthscale', config_restoreS_lengthscale, ierr)
      call MPAS_writeStreamAtt(output_obj % io_stream, 'config_flux_attenuation_coefficient', config_flux_attenuation_coefficient, &
ierr)
      if (config_frazil_ice_formation) then
         call MPAS_writeStreamAtt(output_obj % io_stream, 'config_frazil_ice_formation', 'T', ierr)
      else
         call MPAS_writeStreamAtt(output_obj % io_stream, 'config_frazil_ice_formation', 'F', ierr)
      end if
      call MPAS_writeStreamAtt(output_obj % io_stream, 'config_sw_absorption_type', config_sw_absorption_type, ierr)
      call MPAS_writeStreamAtt(output_obj % io_stream, 'config_jerlov_water_type', config_jerlov_water_type, ierr)
      if (config_fixed_jerlov_weights) then
         call MPAS_writeStreamAtt(output_obj % io_stream, 'config_fixed_jerlov_weights', 'T', ierr)
      else
         call MPAS_writeStreamAtt(output_obj % io_stream, 'config_fixed_jerlov_weights', 'F', ierr)
      end if
      call MPAS_writeStreamAtt(output_obj % io_stream, 'config_vert_tracer_adv', config_vert_tracer_adv, ierr)
      call MPAS_writeStreamAtt(output_obj % io_stream, 'config_vert_tracer_adv_order', config_vert_tracer_adv_order, ierr)
      call MPAS_writeStreamAtt(output_obj % io_stream, 'config_horiz_tracer_adv_order', config_horiz_tracer_adv_order, ierr)
      call MPAS_writeStreamAtt(output_obj % io_stream, 'config_coef_3rd_order', config_coef_3rd_order, ierr)
      if (config_monotonic) then
         call MPAS_writeStreamAtt(output_obj % io_stream, 'config_monotonic', 'T', ierr)
      else
         call MPAS_writeStreamAtt(output_obj % io_stream, 'config_monotonic', 'F', ierr)
      end if
      call MPAS_writeStreamAtt(output_obj % io_stream, 'config_bottom_drag_coeff', config_bottom_drag_coeff, ierr)
      call MPAS_writeStreamAtt(output_obj % io_stream, 'config_pressure_gradient_type', config_pressure_gradient_type, ierr)
      call MPAS_writeStreamAtt(output_obj % io_stream, 'config_density0', config_density0, ierr)
      call MPAS_writeStreamAtt(output_obj % io_stream, 'config_eos_type', config_eos_type, ierr)
      call MPAS_writeStreamAtt(output_obj % io_stream, 'config_eos_linear_alpha', config_eos_linear_alpha, ierr)
      call MPAS_writeStreamAtt(output_obj % io_stream, 'config_eos_linear_beta', config_eos_linear_beta, ierr)
      call MPAS_writeStreamAtt(output_obj % io_stream, 'config_eos_linear_Tref', config_eos_linear_Tref, ierr)
      call MPAS_writeStreamAtt(output_obj % io_stream, 'config_eos_linear_Sref', config_eos_linear_Sref, ierr)
      call MPAS_writeStreamAtt(output_obj % io_stream, 'config_eos_linear_densityref', config_eos_linear_densityref, ierr)
      call MPAS_writeStreamAtt(output_obj % io_stream, 'config_n_ts_iter', config_n_ts_iter, ierr)
      call MPAS_writeStreamAtt(output_obj % io_stream, 'config_n_bcl_iter_beg', config_n_bcl_iter_beg, ierr)
      call MPAS_writeStreamAtt(output_obj % io_stream, 'config_n_bcl_iter_mid', config_n_bcl_iter_mid, ierr)
      call MPAS_writeStreamAtt(output_obj % io_stream, 'config_n_bcl_iter_end', config_n_bcl_iter_end, ierr)
      call MPAS_writeStreamAtt(output_obj % io_stream, 'config_n_btr_subcycles', config_n_btr_subcycles, ierr)
      call MPAS_writeStreamAtt(output_obj % io_stream, 'config_n_btr_cor_iter', config_n_btr_cor_iter, ierr)
      if (config_vel_correction) then
         call MPAS_writeStreamAtt(output_obj % io_stream, 'config_vel_correction', 'T', ierr)
      else
         call MPAS_writeStreamAtt(output_obj % io_stream, 'config_vel_correction', 'F', ierr)
      end if
      call MPAS_writeStreamAtt(output_obj % io_stream, 'config_btr_subcycle_loop_factor', config_btr_subcycle_loop_factor, ierr)
      call MPAS_writeStreamAtt(output_obj % io_stream, 'config_btr_gam1_velWt1', config_btr_gam1_velWt1, ierr)
      call MPAS_writeStreamAtt(output_obj % io_stream, 'config_btr_gam2_SSHWt1', config_btr_gam2_SSHWt1, ierr)
      call MPAS_writeStreamAtt(output_obj % io_stream, 'config_btr_gam3_velWt2', config_btr_gam3_velWt2, ierr)
      if (config_btr_solve_SSH2) then
         call MPAS_writeStreamAtt(output_obj % io_stream, 'config_btr_solve_SSH2', 'T', ierr)
      else
         call MPAS_writeStreamAtt(output_obj % io_stream, 'config_btr_solve_SSH2', 'F', ierr)
      end if
      if (config_conduct_tests) then
         call MPAS_writeStreamAtt(output_obj % io_stream, 'config_conduct_tests', 'T', ierr)
      else
         call MPAS_writeStreamAtt(output_obj % io_stream, 'config_conduct_tests', 'F', ierr)
      end if
      if (config_test_tensors) then
         call MPAS_writeStreamAtt(output_obj % io_stream, 'config_test_tensors', 'T', ierr)
      else
         call MPAS_writeStreamAtt(output_obj % io_stream, 'config_test_tensors', 'F', ierr)
      end if
      call MPAS_writeStreamAtt(output_obj % io_stream, 'config_tensor_test_function', config_tensor_test_function, ierr)
      if (config_check_zlevel_consistency) then
         call MPAS_writeStreamAtt(output_obj % io_stream, 'config_check_zlevel_consistency', 'T', ierr)
      else
         call MPAS_writeStreamAtt(output_obj % io_stream, 'config_check_zlevel_consistency', 'F', ierr)
      end if
      if (config_filter_btr_mode) then
         call MPAS_writeStreamAtt(output_obj % io_stream, 'config_filter_btr_mode', 'T', ierr)
      else
         call MPAS_writeStreamAtt(output_obj % io_stream, 'config_filter_btr_mode', 'F', ierr)
      end if
      if (config_prescribe_velocity) then
         call MPAS_writeStreamAtt(output_obj % io_stream, 'config_prescribe_velocity', 'T', ierr)
      else
         call MPAS_writeStreamAtt(output_obj % io_stream, 'config_prescribe_velocity', 'F', ierr)
      end if
      if (config_prescribe_thickness) then
         call MPAS_writeStreamAtt(output_obj % io_stream, 'config_prescribe_thickness', 'T', ierr)
      else
         call MPAS_writeStreamAtt(output_obj % io_stream, 'config_prescribe_thickness', 'F', ierr)
      end if
      if (config_include_KE_vertex) then
         call MPAS_writeStreamAtt(output_obj % io_stream, 'config_include_KE_vertex', 'T', ierr)
      else
         call MPAS_writeStreamAtt(output_obj % io_stream, 'config_include_KE_vertex', 'F', ierr)
      end if
      if (config_check_tracer_monotonicity) then
         call MPAS_writeStreamAtt(output_obj % io_stream, 'config_check_tracer_monotonicity', 'T', ierr)
      else
         call MPAS_writeStreamAtt(output_obj % io_stream, 'config_check_tracer_monotonicity', 'F', ierr)
      end if
      if (config_disable_thick_all_tend) then
         call MPAS_writeStreamAtt(output_obj % io_stream, 'config_disable_thick_all_tend', 'T', ierr)
      else
         call MPAS_writeStreamAtt(output_obj % io_stream, 'config_disable_thick_all_tend', 'F', ierr)
      end if
      if (config_disable_thick_hadv) then
         call MPAS_writeStreamAtt(output_obj % io_stream, 'config_disable_thick_hadv', 'T', ierr)
      else
         call MPAS_writeStreamAtt(output_obj % io_stream, 'config_disable_thick_hadv', 'F', ierr)
      end if
      if (config_disable_thick_vadv) then
         call MPAS_writeStreamAtt(output_obj % io_stream, 'config_disable_thick_vadv', 'T', ierr)
      else
         call MPAS_writeStreamAtt(output_obj % io_stream, 'config_disable_thick_vadv', 'F', ierr)
      end if
      if (config_disable_thick_sflux) then
         call MPAS_writeStreamAtt(output_obj % io_stream, 'config_disable_thick_sflux', 'T', ierr)
      else
         call MPAS_writeStreamAtt(output_obj % io_stream, 'config_disable_thick_sflux', 'F', ierr)
      end if
      if (config_disable_vel_all_tend) then
         call MPAS_writeStreamAtt(output_obj % io_stream, 'config_disable_vel_all_tend', 'T', ierr)
      else
         call MPAS_writeStreamAtt(output_obj % io_stream, 'config_disable_vel_all_tend', 'F', ierr)
      end if
      if (config_disable_vel_coriolis) then
         call MPAS_writeStreamAtt(output_obj % io_stream, 'config_disable_vel_coriolis', 'T', ierr)
      else
         call MPAS_writeStreamAtt(output_obj % io_stream, 'config_disable_vel_coriolis', 'F', ierr)
      end if
      if (config_disable_vel_pgrad) then
         call MPAS_writeStreamAtt(output_obj % io_stream, 'config_disable_vel_pgrad', 'T', ierr)
      else
         call MPAS_writeStreamAtt(output_obj % io_stream, 'config_disable_vel_pgrad', 'F', ierr)
      end if
      if (config_disable_vel_hmix) then
         call MPAS_writeStreamAtt(output_obj % io_stream, 'config_disable_vel_hmix', 'T', ierr)
      else
         call MPAS_writeStreamAtt(output_obj % io_stream, 'config_disable_vel_hmix', 'F', ierr)
      end if
      if (config_disable_vel_windstress) then
         call MPAS_writeStreamAtt(output_obj % io_stream, 'config_disable_vel_windstress', 'T', ierr)
      else
         call MPAS_writeStreamAtt(output_obj % io_stream, 'config_disable_vel_windstress', 'F', ierr)
      end if
      if (config_disable_vel_vmix) then
         call MPAS_writeStreamAtt(output_obj % io_stream, 'config_disable_vel_vmix', 'T', ierr)
      else
         call MPAS_writeStreamAtt(output_obj % io_stream, 'config_disable_vel_vmix', 'F', ierr)
      end if
      if (config_disable_vel_vadv) then
         call MPAS_writeStreamAtt(output_obj % io_stream, 'config_disable_vel_vadv', 'T', ierr)
      else
         call MPAS_writeStreamAtt(output_obj % io_stream, 'config_disable_vel_vadv', 'F', ierr)
      end if
      if (config_disable_tr_all_tend) then
         call MPAS_writeStreamAtt(output_obj % io_stream, 'config_disable_tr_all_tend', 'T', ierr)
      else
         call MPAS_writeStreamAtt(output_obj % io_stream, 'config_disable_tr_all_tend', 'F', ierr)
      end if
      if (config_disable_tr_adv) then
         call MPAS_writeStreamAtt(output_obj % io_stream, 'config_disable_tr_adv', 'T', ierr)
      else
         call MPAS_writeStreamAtt(output_obj % io_stream, 'config_disable_tr_adv', 'F', ierr)
      end if
      if (config_disable_tr_hmix) then
         call MPAS_writeStreamAtt(output_obj % io_stream, 'config_disable_tr_hmix', 'T', ierr)
      else
         call MPAS_writeStreamAtt(output_obj % io_stream, 'config_disable_tr_hmix', 'F', ierr)
      end if
      if (config_disable_tr_vmix) then
         call MPAS_writeStreamAtt(output_obj % io_stream, 'config_disable_tr_vmix', 'T', ierr)
      else
         call MPAS_writeStreamAtt(output_obj % io_stream, 'config_disable_tr_vmix', 'F', ierr)
      end if
      if (config_disable_tr_sflux) then
         call MPAS_writeStreamAtt(output_obj % io_stream, 'config_disable_tr_sflux', 'T', ierr)
      else
         call MPAS_writeStreamAtt(output_obj % io_stream, 'config_disable_tr_sflux', 'F', ierr)
      end if
 
   end subroutine mpas_io_output_init!}}}

   subroutine mpas_io_output_finalize(output_obj, dminfo)!{{{
 
      implicit none
 
      type (io_output_object), intent(inout) :: output_obj
      type (dm_info), intent(in) :: dminfo
 
      integer :: nferr
 
      call MPAS_closeStream(output_obj % io_stream, nferr)
 
   end subroutine mpas_io_output_finalize!}}}
 
end module mpas_io_output
