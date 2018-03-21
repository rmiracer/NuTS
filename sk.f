        subroutine calc_kurtosis(x, N, skewness, kurtosis, sigma)
        implicit none
        integer N, N_INTERVALS
        integer i, j, k, tau
        double precision sum_serie_mu, sum_serie_sigma
        double precision sum_differences_sk, sum_differences_kurt
        double precision mu, sigma
        double precision x(N), y(N), y_bar(N)
        double precision c_phi(N), temp, temp2
        double precision skewness(N/2), kurtosis(N/2)

c          do tau = 1, N/2 
c          do tau = 1, 1
c          do tau = 300, 1500, 60

c         Statistical moments of points (not two-point differences)
          do tau = 0, 0
            sum_serie_mu    = 0.
            sum_serie_sigma = 0.

c           Calculate mu
            do i = 1, N - tau
c             Statistical moments of points (not two-point differences)
              y(i) = x(i)
c              y(i) = x(i + tau) - x(i)
              sum_serie_mu = sum_serie_mu + y(i)
            enddo
            mu = sum_serie_mu/(N - tau)

c           Calculate sigma
            do i = 1, N - tau
              sum_serie_sigma = sum_serie_sigma + (y(i) - mu)**2
            enddo
            sigma = dsqrt(sum_serie_sigma/(N - tau - 1))

c            if(mod(tau, 1000).eq.0) then
c              write(*, *) 'For tau = ', tau, ', mu = ', mu,
c     *                    ', sigma = ', sigma
c            endif

            sum_differences_sk = 0.
c           Calculate sum for Skewness
            do i = 1, N - tau
              sum_differences_sk = sum_differences_sk
     *                + ((y(i) - mu)/sigma)**3
            enddo

            sum_differences_kurt = 0.
c           Calculate kurtosis
            do i = 1, N - tau
              sum_differences_kurt = sum_differences_kurt
     *                + ((y(i) - mu)/sigma)**4
            enddo

c         Statistical moments of points (not two-point differences)
          skewness(1) = 1./(N-tau)*sum_differences_sk
          kurtosis(1) = 1./(N-tau)*sum_differences_kurt - 3
c          skewness(tau) = 1./(N-tau)*sum_differences_sk
c          kurtosis(tau) = 1./(N-tau)*sum_differences_kurt - 3.

          enddo

        return
        end
