module Equations where


{-#

(1.1  p15)   density = mass / volume

(2.1  p46)   displacement;   delta_x = x_f - x_i

(2.2  p46)   average velocity;   v_avg = delta_x / delta_t

(2.3  p46)   average speed;   v_avg = displacement / delta_t

(2.9  p46)   average acceleration;   a_avg = delta_v / delta_t

Constant acceleration (which could be a=0)

(2.13 p47)   v_f = v_i + a * t

(2.14 p47)   v_avg = (v_i + v_f) / 2

(2.15 p47)   x_f = x_i + (v_i + v_f) * t / 2

(2.16 p47)   x_f = x+i + v_i * t + a * t**2 / 2

(2.17 p47)   v_f**2 = v_i**2 + 2 * a * (x_f - x_i)

(4.1  p88)   displacement vector;   delta_r = r_f - r_i

(4.2  p88)   average velocity;   v_avg = delta_r / delta_t

(4.3  p88)   instantaneous velocity;   v = dr/dt

(4.4  p88)   average acceleration;   a_avg = delta_v / delta_t

(4.21 p89)   centripetal acceleration;   a_c = v**2 / r

(4.22 p89)   period = T = 2 * pi * r / v

(4.23 p89)   angular speed;   w = 2 * pi / T

(5.2 p120)   Newton's second law;   Σ F = m * a

(5.9 p115)   static friction;   f_s <= μ_s * n

(5.10 p116)  kinetic friction;  f_k = μ_k * n

(6.2  p139)  linear resistive force;   R = -b * v

(6.5  p140)  linear terminal speed;   v_T = m * g / b  ('down' due to g)

(6.6  p140)  linear velocity;   v = v_T * (1 - e^{-t / tau})  tau = m / b

(6.7  p141)  square resistive force; R = 0.5 * D * rho * A * v**2
             D -> drag; rho -> density of air; A -> cross sectional area

(6.10 p141)  square terminal speed; v_T = sqrt (2 * m * g / (D * rho * a))

(7.2  p154)  defn dot:   A dot B = |A| * |B| * cos theta

(7.3  p154)  work;   W = F dot delta_r

(7.8 p157)   work;   W = integral of F_ dot dr

(7.9 p158)   force of spring;   F = -k * x

(7.11 p159)  work of spring;   W_s = 0.5 * k * x**2

(7.13 p160)  external work;   W_ext = 0.5 * k * x_f**2 - 0.5 * k * x_i**2

(7.15 p161)  external work;   W_ext = 0.5 * m * v_f**2 - 0.5 * m * v_i**2

(7.16 p161)  kinetic energy;   K = 0.5 * m * v**2

(7.17 p162)  W_ext = K_f - K_i

(7.19 p166)  gravitational PE; U_g = m * g * y

(7.22 p168)  elastic PE; U_s = 0.5 * k * x**2

(7.29 p172)  F_x = -dU/dx

(8.1  p183)  delta_E_system = Σ T
  where
    T = transfer

(8.2  p184)  ΔK + ΔU + ΔE_int = W + Q + T_MW + T_MT + T_ET + T_ER
  where
    K     -> kinetic energy         T_MW -> mechanical waves
    U     -> potential energy       T_MT -> matter transfer
    E_int -> internal energy        T_ET -> electrical transmission
    W     -> work                   T_ER -> electromagnetic radiation
    Q     -> heat transfer

(8.14 p193)  change in E due to friction;   delta_E_int = f_k * d

(8.17 p201)  power;   P = dE/dt

(8.18 p201)  power;   P = F dot v

watt = 1 J/s = 1 kg m**2 / s**3

(9.2  p212)  linear momentum;  p = m * v

(9.3  p212)  newton's 2nd law;   Σ F = dp/dt

(9.9  p216)  impulse;   I = integral of F * dt

(9.13 p217)  impulse; delta_p = I

Total momentum is conserved in *any* collision
Total kinectic energy is conserved only in elastic collision (e.g. perfect bounce)

(9.15 p220)  perfectly inelastic; v_f = (m1*v1 + m2*v2) / (m1 + m2)  (e.g. stuck together)

(9.16 p221)  all collisions (momentum);   m1 * v1_i + m2 * v2_i = m1 * v1_f + m2 * v2_f

(9.17 p221)  elastic (kinetic);   0.5 * m1 * v1_i**2 + 0.5 * m2 * v2_i**2 = 0.5 * m1 * v1_f**2 + 0.5 * m2 * v2_f**2

(9.20 p221)  elastic; v1_i - v2_i = -1 * (v1_f - v2_f)


#-}