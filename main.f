        implicit none
        integer N, W_SIZE
c       ACE data
c        parameter(N = 1800, W_SIZE = 500)
c       Cluster data
        parameter(N = 2604207, W_SIZE = 10000)
        integer i, j
        double precision window(W_SIZE), buffer(N), t(N)
        double precision skewness(W_SIZE/2)
        double precision kurtosis(W_SIZE/2)
        double precision sigma
        double precision avrg_skew, avrg_kurt
        double precision m_trend, n_trend
        external calc_kurtosis

        open(1, file='time_series.dat', status='old')
        open(2, file='w_sk_kurt.dat', status='unknown')
        open(3, file='sel_ts.dat', status='unknown')

        write(*, *) 'Reading input file...'
        do i = 1, N
c          read(1, *) t(i), buffer(i)
          read(1, *) buffer(i)
c         Missing data? Don't use this!
c          if(buffer(i).lt.0.and.i.ne.1) then
c            buffer(i) = buffer(i-1)
c          endif
          if(348879.lt.i.and.i.lt.671749) then
            write(3, *) i, buffer(i)
          endif

        enddo
        write(*, *) 'Done.'

        do i = 1, N - W_SIZE, 400
c        do i = 1, N - W_SIZE, 400
c          write(*, *) i

          do j = 1, W_SIZE
            window(j) = buffer(i + j - 1)
          enddo

c          m_trend = (window(W_SIZE) - window(1))/(W_SIZE - 1)
c          n_trend = window(1)

c          do j = 1, W_SIZE
c            window(j) = window(j) - (m_trend*j + n_trend)
c            write(*, *) j, window(j)
c          enddo
c          write(*, *) '======================='


          call calc_kurtosis(window, W_SIZE, skewness, kurtosis, sigma)

          avrg_skew = 0.
          avrg_kurt = 0.

c         Statistical moments of points (not two-point differences)
          if(348879.lt.i.and.i.lt.671749) then
            write(2, *) (((i-348879)+W_SIZE/2)*0.044)/3600, skewness(1),
     c                    kurtosis(1), sigma
          endif

c          do j = 300, 1500, 60
c            avrg_skew = avrg_skew + skewness(j)
c            avrg_kurt = avrg_kurt + kurtosis(j)
c          enddo

c          write(2, *) t(i), avrg_skew/((1500 - 300)/60),
c     *                      avrg_kurt/((1500 - 300)/60)
c
        enddo

        close(1)
        close(2)

        stop
        end
