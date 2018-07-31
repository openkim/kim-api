   switch(GetComputeIndex(isComputeProcess_dEdr,
                          isComputeProcess_d2Edr2,
                          isComputeEnergy,
                          isComputeForces,
                          isComputeParticleEnergy,
                          isComputeVirial,
                          isComputeParticleVirial,
                          isShift))
   {
      case 0:
         ier = Compute< false, false,
                        false, false,
                        false, false,
                        false, false >(
                  modelCompute,
                  modelComputeArguments,
                  particleSpeciesCodes,
                  particleContributing,
                  coordinates,
                  energy,
                  forces,
                  particleEnergy,
                  *virial,
                  particleVirial);
         break;
      case 1:
         ier = Compute< false, false,
                        false, false,
                        false, false,
                        false, true >(
                  modelCompute,
                  modelComputeArguments,
                  particleSpeciesCodes,
                  particleContributing,
                  coordinates,
                  energy,
                  forces,
                  particleEnergy,
                  *virial,
                  particleVirial);
         break;
      case 2:
         ier = Compute< false, false,
                        false, false,
                        false, false,
                        true, false >(
                  modelCompute,
                  modelComputeArguments,
                  particleSpeciesCodes,
                  particleContributing,
                  coordinates,
                  energy,
                  forces,
                  particleEnergy,
                  *virial,
                  particleVirial);
         break;
      case 3:
         ier = Compute< false, false,
                        false, false,
                        false, false,
                        true, true >(
                  modelCompute,
                  modelComputeArguments,
                  particleSpeciesCodes,
                  particleContributing,
                  coordinates,
                  energy,
                  forces,
                  particleEnergy,
                  *virial,
                  particleVirial);
         break;
      case 4:
         ier = Compute< false, false,
                        false, false,
                        false, true,
                        false, false >(
                  modelCompute,
                  modelComputeArguments,
                  particleSpeciesCodes,
                  particleContributing,
                  coordinates,
                  energy,
                  forces,
                  particleEnergy,
                  *virial,
                  particleVirial);
         break;
      case 5:
         ier = Compute< false, false,
                        false, false,
                        false, true,
                        false, true >(
                  modelCompute,
                  modelComputeArguments,
                  particleSpeciesCodes,
                  particleContributing,
                  coordinates,
                  energy,
                  forces,
                  particleEnergy,
                  *virial,
                  particleVirial);
         break;
      case 6:
         ier = Compute< false, false,
                        false, false,
                        false, true,
                        true, false >(
                  modelCompute,
                  modelComputeArguments,
                  particleSpeciesCodes,
                  particleContributing,
                  coordinates,
                  energy,
                  forces,
                  particleEnergy,
                  *virial,
                  particleVirial);
         break;
      case 7:
         ier = Compute< false, false,
                        false, false,
                        false, true,
                        true, true >(
                  modelCompute,
                  modelComputeArguments,
                  particleSpeciesCodes,
                  particleContributing,
                  coordinates,
                  energy,
                  forces,
                  particleEnergy,
                  *virial,
                  particleVirial);
         break;
      case 8:
         ier = Compute< false, false,
                        false, false,
                        true, false,
                        false, false >(
                  modelCompute,
                  modelComputeArguments,
                  particleSpeciesCodes,
                  particleContributing,
                  coordinates,
                  energy,
                  forces,
                  particleEnergy,
                  *virial,
                  particleVirial);
         break;
      case 9:
         ier = Compute< false, false,
                        false, false,
                        true, false,
                        false, true >(
                  modelCompute,
                  modelComputeArguments,
                  particleSpeciesCodes,
                  particleContributing,
                  coordinates,
                  energy,
                  forces,
                  particleEnergy,
                  *virial,
                  particleVirial);
         break;
      case 10:
         ier = Compute< false, false,
                        false, false,
                        true, false,
                        true, false >(
                  modelCompute,
                  modelComputeArguments,
                  particleSpeciesCodes,
                  particleContributing,
                  coordinates,
                  energy,
                  forces,
                  particleEnergy,
                  *virial,
                  particleVirial);
         break;
      case 11:
         ier = Compute< false, false,
                        false, false,
                        true, false,
                        true, true >(
                  modelCompute,
                  modelComputeArguments,
                  particleSpeciesCodes,
                  particleContributing,
                  coordinates,
                  energy,
                  forces,
                  particleEnergy,
                  *virial,
                  particleVirial);
         break;
      case 12:
         ier = Compute< false, false,
                        false, false,
                        true, true,
                        false, false >(
                  modelCompute,
                  modelComputeArguments,
                  particleSpeciesCodes,
                  particleContributing,
                  coordinates,
                  energy,
                  forces,
                  particleEnergy,
                  *virial,
                  particleVirial);
         break;
      case 13:
         ier = Compute< false, false,
                        false, false,
                        true, true,
                        false, true >(
                  modelCompute,
                  modelComputeArguments,
                  particleSpeciesCodes,
                  particleContributing,
                  coordinates,
                  energy,
                  forces,
                  particleEnergy,
                  *virial,
                  particleVirial);
         break;
      case 14:
         ier = Compute< false, false,
                        false, false,
                        true, true,
                        true, false >(
                  modelCompute,
                  modelComputeArguments,
                  particleSpeciesCodes,
                  particleContributing,
                  coordinates,
                  energy,
                  forces,
                  particleEnergy,
                  *virial,
                  particleVirial);
         break;
      case 15:
         ier = Compute< false, false,
                        false, false,
                        true, true,
                        true, true >(
                  modelCompute,
                  modelComputeArguments,
                  particleSpeciesCodes,
                  particleContributing,
                  coordinates,
                  energy,
                  forces,
                  particleEnergy,
                  *virial,
                  particleVirial);
         break;
      case 16:
         ier = Compute< false, false,
                        false, true,
                        false, false,
                        false, false >(
                  modelCompute,
                  modelComputeArguments,
                  particleSpeciesCodes,
                  particleContributing,
                  coordinates,
                  energy,
                  forces,
                  particleEnergy,
                  *virial,
                  particleVirial);
         break;
      case 17:
         ier = Compute< false, false,
                        false, true,
                        false, false,
                        false, true >(
                  modelCompute,
                  modelComputeArguments,
                  particleSpeciesCodes,
                  particleContributing,
                  coordinates,
                  energy,
                  forces,
                  particleEnergy,
                  *virial,
                  particleVirial);
         break;
      case 18:
         ier = Compute< false, false,
                        false, true,
                        false, false,
                        true, false >(
                  modelCompute,
                  modelComputeArguments,
                  particleSpeciesCodes,
                  particleContributing,
                  coordinates,
                  energy,
                  forces,
                  particleEnergy,
                  *virial,
                  particleVirial);
         break;
      case 19:
         ier = Compute< false, false,
                        false, true,
                        false, false,
                        true, true >(
                  modelCompute,
                  modelComputeArguments,
                  particleSpeciesCodes,
                  particleContributing,
                  coordinates,
                  energy,
                  forces,
                  particleEnergy,
                  *virial,
                  particleVirial);
         break;
      case 20:
         ier = Compute< false, false,
                        false, true,
                        false, true,
                        false, false >(
                  modelCompute,
                  modelComputeArguments,
                  particleSpeciesCodes,
                  particleContributing,
                  coordinates,
                  energy,
                  forces,
                  particleEnergy,
                  *virial,
                  particleVirial);
         break;
      case 21:
         ier = Compute< false, false,
                        false, true,
                        false, true,
                        false, true >(
                  modelCompute,
                  modelComputeArguments,
                  particleSpeciesCodes,
                  particleContributing,
                  coordinates,
                  energy,
                  forces,
                  particleEnergy,
                  *virial,
                  particleVirial);
         break;
      case 22:
         ier = Compute< false, false,
                        false, true,
                        false, true,
                        true, false >(
                  modelCompute,
                  modelComputeArguments,
                  particleSpeciesCodes,
                  particleContributing,
                  coordinates,
                  energy,
                  forces,
                  particleEnergy,
                  *virial,
                  particleVirial);
         break;
      case 23:
         ier = Compute< false, false,
                        false, true,
                        false, true,
                        true, true >(
                  modelCompute,
                  modelComputeArguments,
                  particleSpeciesCodes,
                  particleContributing,
                  coordinates,
                  energy,
                  forces,
                  particleEnergy,
                  *virial,
                  particleVirial);
         break;
      case 24:
         ier = Compute< false, false,
                        false, true,
                        true, false,
                        false, false >(
                  modelCompute,
                  modelComputeArguments,
                  particleSpeciesCodes,
                  particleContributing,
                  coordinates,
                  energy,
                  forces,
                  particleEnergy,
                  *virial,
                  particleVirial);
         break;
      case 25:
         ier = Compute< false, false,
                        false, true,
                        true, false,
                        false, true >(
                  modelCompute,
                  modelComputeArguments,
                  particleSpeciesCodes,
                  particleContributing,
                  coordinates,
                  energy,
                  forces,
                  particleEnergy,
                  *virial,
                  particleVirial);
         break;
      case 26:
         ier = Compute< false, false,
                        false, true,
                        true, false,
                        true, false >(
                  modelCompute,
                  modelComputeArguments,
                  particleSpeciesCodes,
                  particleContributing,
                  coordinates,
                  energy,
                  forces,
                  particleEnergy,
                  *virial,
                  particleVirial);
         break;
      case 27:
         ier = Compute< false, false,
                        false, true,
                        true, false,
                        true, true >(
                  modelCompute,
                  modelComputeArguments,
                  particleSpeciesCodes,
                  particleContributing,
                  coordinates,
                  energy,
                  forces,
                  particleEnergy,
                  *virial,
                  particleVirial);
         break;
      case 28:
         ier = Compute< false, false,
                        false, true,
                        true, true,
                        false, false >(
                  modelCompute,
                  modelComputeArguments,
                  particleSpeciesCodes,
                  particleContributing,
                  coordinates,
                  energy,
                  forces,
                  particleEnergy,
                  *virial,
                  particleVirial);
         break;
      case 29:
         ier = Compute< false, false,
                        false, true,
                        true, true,
                        false, true >(
                  modelCompute,
                  modelComputeArguments,
                  particleSpeciesCodes,
                  particleContributing,
                  coordinates,
                  energy,
                  forces,
                  particleEnergy,
                  *virial,
                  particleVirial);
         break;
      case 30:
         ier = Compute< false, false,
                        false, true,
                        true, true,
                        true, false >(
                  modelCompute,
                  modelComputeArguments,
                  particleSpeciesCodes,
                  particleContributing,
                  coordinates,
                  energy,
                  forces,
                  particleEnergy,
                  *virial,
                  particleVirial);
         break;
      case 31:
         ier = Compute< false, false,
                        false, true,
                        true, true,
                        true, true >(
                  modelCompute,
                  modelComputeArguments,
                  particleSpeciesCodes,
                  particleContributing,
                  coordinates,
                  energy,
                  forces,
                  particleEnergy,
                  *virial,
                  particleVirial);
         break;
      case 32:
         ier = Compute< false, false,
                        true, false,
                        false, false,
                        false, false >(
                  modelCompute,
                  modelComputeArguments,
                  particleSpeciesCodes,
                  particleContributing,
                  coordinates,
                  energy,
                  forces,
                  particleEnergy,
                  *virial,
                  particleVirial);
         break;
      case 33:
         ier = Compute< false, false,
                        true, false,
                        false, false,
                        false, true >(
                  modelCompute,
                  modelComputeArguments,
                  particleSpeciesCodes,
                  particleContributing,
                  coordinates,
                  energy,
                  forces,
                  particleEnergy,
                  *virial,
                  particleVirial);
         break;
      case 34:
         ier = Compute< false, false,
                        true, false,
                        false, false,
                        true, false >(
                  modelCompute,
                  modelComputeArguments,
                  particleSpeciesCodes,
                  particleContributing,
                  coordinates,
                  energy,
                  forces,
                  particleEnergy,
                  *virial,
                  particleVirial);
         break;
      case 35:
         ier = Compute< false, false,
                        true, false,
                        false, false,
                        true, true >(
                  modelCompute,
                  modelComputeArguments,
                  particleSpeciesCodes,
                  particleContributing,
                  coordinates,
                  energy,
                  forces,
                  particleEnergy,
                  *virial,
                  particleVirial);
         break;
      case 36:
         ier = Compute< false, false,
                        true, false,
                        false, true,
                        false, false >(
                  modelCompute,
                  modelComputeArguments,
                  particleSpeciesCodes,
                  particleContributing,
                  coordinates,
                  energy,
                  forces,
                  particleEnergy,
                  *virial,
                  particleVirial);
         break;
      case 37:
         ier = Compute< false, false,
                        true, false,
                        false, true,
                        false, true >(
                  modelCompute,
                  modelComputeArguments,
                  particleSpeciesCodes,
                  particleContributing,
                  coordinates,
                  energy,
                  forces,
                  particleEnergy,
                  *virial,
                  particleVirial);
         break;
      case 38:
         ier = Compute< false, false,
                        true, false,
                        false, true,
                        true, false >(
                  modelCompute,
                  modelComputeArguments,
                  particleSpeciesCodes,
                  particleContributing,
                  coordinates,
                  energy,
                  forces,
                  particleEnergy,
                  *virial,
                  particleVirial);
         break;
      case 39:
         ier = Compute< false, false,
                        true, false,
                        false, true,
                        true, true >(
                  modelCompute,
                  modelComputeArguments,
                  particleSpeciesCodes,
                  particleContributing,
                  coordinates,
                  energy,
                  forces,
                  particleEnergy,
                  *virial,
                  particleVirial);
         break;
      case 40:
         ier = Compute< false, false,
                        true, false,
                        true, false,
                        false, false >(
                  modelCompute,
                  modelComputeArguments,
                  particleSpeciesCodes,
                  particleContributing,
                  coordinates,
                  energy,
                  forces,
                  particleEnergy,
                  *virial,
                  particleVirial);
         break;
      case 41:
         ier = Compute< false, false,
                        true, false,
                        true, false,
                        false, true >(
                  modelCompute,
                  modelComputeArguments,
                  particleSpeciesCodes,
                  particleContributing,
                  coordinates,
                  energy,
                  forces,
                  particleEnergy,
                  *virial,
                  particleVirial);
         break;
      case 42:
         ier = Compute< false, false,
                        true, false,
                        true, false,
                        true, false >(
                  modelCompute,
                  modelComputeArguments,
                  particleSpeciesCodes,
                  particleContributing,
                  coordinates,
                  energy,
                  forces,
                  particleEnergy,
                  *virial,
                  particleVirial);
         break;
      case 43:
         ier = Compute< false, false,
                        true, false,
                        true, false,
                        true, true >(
                  modelCompute,
                  modelComputeArguments,
                  particleSpeciesCodes,
                  particleContributing,
                  coordinates,
                  energy,
                  forces,
                  particleEnergy,
                  *virial,
                  particleVirial);
         break;
      case 44:
         ier = Compute< false, false,
                        true, false,
                        true, true,
                        false, false >(
                  modelCompute,
                  modelComputeArguments,
                  particleSpeciesCodes,
                  particleContributing,
                  coordinates,
                  energy,
                  forces,
                  particleEnergy,
                  *virial,
                  particleVirial);
         break;
      case 45:
         ier = Compute< false, false,
                        true, false,
                        true, true,
                        false, true >(
                  modelCompute,
                  modelComputeArguments,
                  particleSpeciesCodes,
                  particleContributing,
                  coordinates,
                  energy,
                  forces,
                  particleEnergy,
                  *virial,
                  particleVirial);
         break;
      case 46:
         ier = Compute< false, false,
                        true, false,
                        true, true,
                        true, false >(
                  modelCompute,
                  modelComputeArguments,
                  particleSpeciesCodes,
                  particleContributing,
                  coordinates,
                  energy,
                  forces,
                  particleEnergy,
                  *virial,
                  particleVirial);
         break;
      case 47:
         ier = Compute< false, false,
                        true, false,
                        true, true,
                        true, true >(
                  modelCompute,
                  modelComputeArguments,
                  particleSpeciesCodes,
                  particleContributing,
                  coordinates,
                  energy,
                  forces,
                  particleEnergy,
                  *virial,
                  particleVirial);
         break;
      case 48:
         ier = Compute< false, false,
                        true, true,
                        false, false,
                        false, false >(
                  modelCompute,
                  modelComputeArguments,
                  particleSpeciesCodes,
                  particleContributing,
                  coordinates,
                  energy,
                  forces,
                  particleEnergy,
                  *virial,
                  particleVirial);
         break;
      case 49:
         ier = Compute< false, false,
                        true, true,
                        false, false,
                        false, true >(
                  modelCompute,
                  modelComputeArguments,
                  particleSpeciesCodes,
                  particleContributing,
                  coordinates,
                  energy,
                  forces,
                  particleEnergy,
                  *virial,
                  particleVirial);
         break;
      case 50:
         ier = Compute< false, false,
                        true, true,
                        false, false,
                        true, false >(
                  modelCompute,
                  modelComputeArguments,
                  particleSpeciesCodes,
                  particleContributing,
                  coordinates,
                  energy,
                  forces,
                  particleEnergy,
                  *virial,
                  particleVirial);
         break;
      case 51:
         ier = Compute< false, false,
                        true, true,
                        false, false,
                        true, true >(
                  modelCompute,
                  modelComputeArguments,
                  particleSpeciesCodes,
                  particleContributing,
                  coordinates,
                  energy,
                  forces,
                  particleEnergy,
                  *virial,
                  particleVirial);
         break;
      case 52:
         ier = Compute< false, false,
                        true, true,
                        false, true,
                        false, false >(
                  modelCompute,
                  modelComputeArguments,
                  particleSpeciesCodes,
                  particleContributing,
                  coordinates,
                  energy,
                  forces,
                  particleEnergy,
                  *virial,
                  particleVirial);
         break;
      case 53:
         ier = Compute< false, false,
                        true, true,
                        false, true,
                        false, true >(
                  modelCompute,
                  modelComputeArguments,
                  particleSpeciesCodes,
                  particleContributing,
                  coordinates,
                  energy,
                  forces,
                  particleEnergy,
                  *virial,
                  particleVirial);
         break;
      case 54:
         ier = Compute< false, false,
                        true, true,
                        false, true,
                        true, false >(
                  modelCompute,
                  modelComputeArguments,
                  particleSpeciesCodes,
                  particleContributing,
                  coordinates,
                  energy,
                  forces,
                  particleEnergy,
                  *virial,
                  particleVirial);
         break;
      case 55:
         ier = Compute< false, false,
                        true, true,
                        false, true,
                        true, true >(
                  modelCompute,
                  modelComputeArguments,
                  particleSpeciesCodes,
                  particleContributing,
                  coordinates,
                  energy,
                  forces,
                  particleEnergy,
                  *virial,
                  particleVirial);
         break;
      case 56:
         ier = Compute< false, false,
                        true, true,
                        true, false,
                        false, false >(
                  modelCompute,
                  modelComputeArguments,
                  particleSpeciesCodes,
                  particleContributing,
                  coordinates,
                  energy,
                  forces,
                  particleEnergy,
                  *virial,
                  particleVirial);
         break;
      case 57:
         ier = Compute< false, false,
                        true, true,
                        true, false,
                        false, true >(
                  modelCompute,
                  modelComputeArguments,
                  particleSpeciesCodes,
                  particleContributing,
                  coordinates,
                  energy,
                  forces,
                  particleEnergy,
                  *virial,
                  particleVirial);
         break;
      case 58:
         ier = Compute< false, false,
                        true, true,
                        true, false,
                        true, false >(
                  modelCompute,
                  modelComputeArguments,
                  particleSpeciesCodes,
                  particleContributing,
                  coordinates,
                  energy,
                  forces,
                  particleEnergy,
                  *virial,
                  particleVirial);
         break;
      case 59:
         ier = Compute< false, false,
                        true, true,
                        true, false,
                        true, true >(
                  modelCompute,
                  modelComputeArguments,
                  particleSpeciesCodes,
                  particleContributing,
                  coordinates,
                  energy,
                  forces,
                  particleEnergy,
                  *virial,
                  particleVirial);
         break;
      case 60:
         ier = Compute< false, false,
                        true, true,
                        true, true,
                        false, false >(
                  modelCompute,
                  modelComputeArguments,
                  particleSpeciesCodes,
                  particleContributing,
                  coordinates,
                  energy,
                  forces,
                  particleEnergy,
                  *virial,
                  particleVirial);
         break;
      case 61:
         ier = Compute< false, false,
                        true, true,
                        true, true,
                        false, true >(
                  modelCompute,
                  modelComputeArguments,
                  particleSpeciesCodes,
                  particleContributing,
                  coordinates,
                  energy,
                  forces,
                  particleEnergy,
                  *virial,
                  particleVirial);
         break;
      case 62:
         ier = Compute< false, false,
                        true, true,
                        true, true,
                        true, false >(
                  modelCompute,
                  modelComputeArguments,
                  particleSpeciesCodes,
                  particleContributing,
                  coordinates,
                  energy,
                  forces,
                  particleEnergy,
                  *virial,
                  particleVirial);
         break;
      case 63:
         ier = Compute< false, false,
                        true, true,
                        true, true,
                        true, true >(
                  modelCompute,
                  modelComputeArguments,
                  particleSpeciesCodes,
                  particleContributing,
                  coordinates,
                  energy,
                  forces,
                  particleEnergy,
                  *virial,
                  particleVirial);
         break;
      case 64:
         ier = Compute< false, true,
                        false, false,
                        false, false,
                        false, false >(
                  modelCompute,
                  modelComputeArguments,
                  particleSpeciesCodes,
                  particleContributing,
                  coordinates,
                  energy,
                  forces,
                  particleEnergy,
                  *virial,
                  particleVirial);
         break;
      case 65:
         ier = Compute< false, true,
                        false, false,
                        false, false,
                        false, true >(
                  modelCompute,
                  modelComputeArguments,
                  particleSpeciesCodes,
                  particleContributing,
                  coordinates,
                  energy,
                  forces,
                  particleEnergy,
                  *virial,
                  particleVirial);
         break;
      case 66:
         ier = Compute< false, true,
                        false, false,
                        false, false,
                        true, false >(
                  modelCompute,
                  modelComputeArguments,
                  particleSpeciesCodes,
                  particleContributing,
                  coordinates,
                  energy,
                  forces,
                  particleEnergy,
                  *virial,
                  particleVirial);
         break;
      case 67:
         ier = Compute< false, true,
                        false, false,
                        false, false,
                        true, true >(
                  modelCompute,
                  modelComputeArguments,
                  particleSpeciesCodes,
                  particleContributing,
                  coordinates,
                  energy,
                  forces,
                  particleEnergy,
                  *virial,
                  particleVirial);
         break;
      case 68:
         ier = Compute< false, true,
                        false, false,
                        false, true,
                        false, false >(
                  modelCompute,
                  modelComputeArguments,
                  particleSpeciesCodes,
                  particleContributing,
                  coordinates,
                  energy,
                  forces,
                  particleEnergy,
                  *virial,
                  particleVirial);
         break;
      case 69:
         ier = Compute< false, true,
                        false, false,
                        false, true,
                        false, true >(
                  modelCompute,
                  modelComputeArguments,
                  particleSpeciesCodes,
                  particleContributing,
                  coordinates,
                  energy,
                  forces,
                  particleEnergy,
                  *virial,
                  particleVirial);
         break;
      case 70:
         ier = Compute< false, true,
                        false, false,
                        false, true,
                        true, false >(
                  modelCompute,
                  modelComputeArguments,
                  particleSpeciesCodes,
                  particleContributing,
                  coordinates,
                  energy,
                  forces,
                  particleEnergy,
                  *virial,
                  particleVirial);
         break;
      case 71:
         ier = Compute< false, true,
                        false, false,
                        false, true,
                        true, true >(
                  modelCompute,
                  modelComputeArguments,
                  particleSpeciesCodes,
                  particleContributing,
                  coordinates,
                  energy,
                  forces,
                  particleEnergy,
                  *virial,
                  particleVirial);
         break;
      case 72:
         ier = Compute< false, true,
                        false, false,
                        true, false,
                        false, false >(
                  modelCompute,
                  modelComputeArguments,
                  particleSpeciesCodes,
                  particleContributing,
                  coordinates,
                  energy,
                  forces,
                  particleEnergy,
                  *virial,
                  particleVirial);
         break;
      case 73:
         ier = Compute< false, true,
                        false, false,
                        true, false,
                        false, true >(
                  modelCompute,
                  modelComputeArguments,
                  particleSpeciesCodes,
                  particleContributing,
                  coordinates,
                  energy,
                  forces,
                  particleEnergy,
                  *virial,
                  particleVirial);
         break;
      case 74:
         ier = Compute< false, true,
                        false, false,
                        true, false,
                        true, false >(
                  modelCompute,
                  modelComputeArguments,
                  particleSpeciesCodes,
                  particleContributing,
                  coordinates,
                  energy,
                  forces,
                  particleEnergy,
                  *virial,
                  particleVirial);
         break;
      case 75:
         ier = Compute< false, true,
                        false, false,
                        true, false,
                        true, true >(
                  modelCompute,
                  modelComputeArguments,
                  particleSpeciesCodes,
                  particleContributing,
                  coordinates,
                  energy,
                  forces,
                  particleEnergy,
                  *virial,
                  particleVirial);
         break;
      case 76:
         ier = Compute< false, true,
                        false, false,
                        true, true,
                        false, false >(
                  modelCompute,
                  modelComputeArguments,
                  particleSpeciesCodes,
                  particleContributing,
                  coordinates,
                  energy,
                  forces,
                  particleEnergy,
                  *virial,
                  particleVirial);
         break;
      case 77:
         ier = Compute< false, true,
                        false, false,
                        true, true,
                        false, true >(
                  modelCompute,
                  modelComputeArguments,
                  particleSpeciesCodes,
                  particleContributing,
                  coordinates,
                  energy,
                  forces,
                  particleEnergy,
                  *virial,
                  particleVirial);
         break;
      case 78:
         ier = Compute< false, true,
                        false, false,
                        true, true,
                        true, false >(
                  modelCompute,
                  modelComputeArguments,
                  particleSpeciesCodes,
                  particleContributing,
                  coordinates,
                  energy,
                  forces,
                  particleEnergy,
                  *virial,
                  particleVirial);
         break;
      case 79:
         ier = Compute< false, true,
                        false, false,
                        true, true,
                        true, true >(
                  modelCompute,
                  modelComputeArguments,
                  particleSpeciesCodes,
                  particleContributing,
                  coordinates,
                  energy,
                  forces,
                  particleEnergy,
                  *virial,
                  particleVirial);
         break;
      case 80:
         ier = Compute< false, true,
                        false, true,
                        false, false,
                        false, false >(
                  modelCompute,
                  modelComputeArguments,
                  particleSpeciesCodes,
                  particleContributing,
                  coordinates,
                  energy,
                  forces,
                  particleEnergy,
                  *virial,
                  particleVirial);
         break;
      case 81:
         ier = Compute< false, true,
                        false, true,
                        false, false,
                        false, true >(
                  modelCompute,
                  modelComputeArguments,
                  particleSpeciesCodes,
                  particleContributing,
                  coordinates,
                  energy,
                  forces,
                  particleEnergy,
                  *virial,
                  particleVirial);
         break;
      case 82:
         ier = Compute< false, true,
                        false, true,
                        false, false,
                        true, false >(
                  modelCompute,
                  modelComputeArguments,
                  particleSpeciesCodes,
                  particleContributing,
                  coordinates,
                  energy,
                  forces,
                  particleEnergy,
                  *virial,
                  particleVirial);
         break;
      case 83:
         ier = Compute< false, true,
                        false, true,
                        false, false,
                        true, true >(
                  modelCompute,
                  modelComputeArguments,
                  particleSpeciesCodes,
                  particleContributing,
                  coordinates,
                  energy,
                  forces,
                  particleEnergy,
                  *virial,
                  particleVirial);
         break;
      case 84:
         ier = Compute< false, true,
                        false, true,
                        false, true,
                        false, false >(
                  modelCompute,
                  modelComputeArguments,
                  particleSpeciesCodes,
                  particleContributing,
                  coordinates,
                  energy,
                  forces,
                  particleEnergy,
                  *virial,
                  particleVirial);
         break;
      case 85:
         ier = Compute< false, true,
                        false, true,
                        false, true,
                        false, true >(
                  modelCompute,
                  modelComputeArguments,
                  particleSpeciesCodes,
                  particleContributing,
                  coordinates,
                  energy,
                  forces,
                  particleEnergy,
                  *virial,
                  particleVirial);
         break;
      case 86:
         ier = Compute< false, true,
                        false, true,
                        false, true,
                        true, false >(
                  modelCompute,
                  modelComputeArguments,
                  particleSpeciesCodes,
                  particleContributing,
                  coordinates,
                  energy,
                  forces,
                  particleEnergy,
                  *virial,
                  particleVirial);
         break;
      case 87:
         ier = Compute< false, true,
                        false, true,
                        false, true,
                        true, true >(
                  modelCompute,
                  modelComputeArguments,
                  particleSpeciesCodes,
                  particleContributing,
                  coordinates,
                  energy,
                  forces,
                  particleEnergy,
                  *virial,
                  particleVirial);
         break;
      case 88:
         ier = Compute< false, true,
                        false, true,
                        true, false,
                        false, false >(
                  modelCompute,
                  modelComputeArguments,
                  particleSpeciesCodes,
                  particleContributing,
                  coordinates,
                  energy,
                  forces,
                  particleEnergy,
                  *virial,
                  particleVirial);
         break;
      case 89:
         ier = Compute< false, true,
                        false, true,
                        true, false,
                        false, true >(
                  modelCompute,
                  modelComputeArguments,
                  particleSpeciesCodes,
                  particleContributing,
                  coordinates,
                  energy,
                  forces,
                  particleEnergy,
                  *virial,
                  particleVirial);
         break;
      case 90:
         ier = Compute< false, true,
                        false, true,
                        true, false,
                        true, false >(
                  modelCompute,
                  modelComputeArguments,
                  particleSpeciesCodes,
                  particleContributing,
                  coordinates,
                  energy,
                  forces,
                  particleEnergy,
                  *virial,
                  particleVirial);
         break;
      case 91:
         ier = Compute< false, true,
                        false, true,
                        true, false,
                        true, true >(
                  modelCompute,
                  modelComputeArguments,
                  particleSpeciesCodes,
                  particleContributing,
                  coordinates,
                  energy,
                  forces,
                  particleEnergy,
                  *virial,
                  particleVirial);
         break;
      case 92:
         ier = Compute< false, true,
                        false, true,
                        true, true,
                        false, false >(
                  modelCompute,
                  modelComputeArguments,
                  particleSpeciesCodes,
                  particleContributing,
                  coordinates,
                  energy,
                  forces,
                  particleEnergy,
                  *virial,
                  particleVirial);
         break;
      case 93:
         ier = Compute< false, true,
                        false, true,
                        true, true,
                        false, true >(
                  modelCompute,
                  modelComputeArguments,
                  particleSpeciesCodes,
                  particleContributing,
                  coordinates,
                  energy,
                  forces,
                  particleEnergy,
                  *virial,
                  particleVirial);
         break;
      case 94:
         ier = Compute< false, true,
                        false, true,
                        true, true,
                        true, false >(
                  modelCompute,
                  modelComputeArguments,
                  particleSpeciesCodes,
                  particleContributing,
                  coordinates,
                  energy,
                  forces,
                  particleEnergy,
                  *virial,
                  particleVirial);
         break;
      case 95:
         ier = Compute< false, true,
                        false, true,
                        true, true,
                        true, true >(
                  modelCompute,
                  modelComputeArguments,
                  particleSpeciesCodes,
                  particleContributing,
                  coordinates,
                  energy,
                  forces,
                  particleEnergy,
                  *virial,
                  particleVirial);
         break;
      case 96:
         ier = Compute< false, true,
                        true, false,
                        false, false,
                        false, false >(
                  modelCompute,
                  modelComputeArguments,
                  particleSpeciesCodes,
                  particleContributing,
                  coordinates,
                  energy,
                  forces,
                  particleEnergy,
                  *virial,
                  particleVirial);
         break;
      case 97:
         ier = Compute< false, true,
                        true, false,
                        false, false,
                        false, true >(
                  modelCompute,
                  modelComputeArguments,
                  particleSpeciesCodes,
                  particleContributing,
                  coordinates,
                  energy,
                  forces,
                  particleEnergy,
                  *virial,
                  particleVirial);
         break;
      case 98:
         ier = Compute< false, true,
                        true, false,
                        false, false,
                        true, false >(
                  modelCompute,
                  modelComputeArguments,
                  particleSpeciesCodes,
                  particleContributing,
                  coordinates,
                  energy,
                  forces,
                  particleEnergy,
                  *virial,
                  particleVirial);
         break;
      case 99:
         ier = Compute< false, true,
                        true, false,
                        false, false,
                        true, true >(
                  modelCompute,
                  modelComputeArguments,
                  particleSpeciesCodes,
                  particleContributing,
                  coordinates,
                  energy,
                  forces,
                  particleEnergy,
                  *virial,
                  particleVirial);
         break;
      case 100:
         ier = Compute< false, true,
                        true, false,
                        false, true,
                        false, false >(
                  modelCompute,
                  modelComputeArguments,
                  particleSpeciesCodes,
                  particleContributing,
                  coordinates,
                  energy,
                  forces,
                  particleEnergy,
                  *virial,
                  particleVirial);
         break;
      case 101:
         ier = Compute< false, true,
                        true, false,
                        false, true,
                        false, true >(
                  modelCompute,
                  modelComputeArguments,
                  particleSpeciesCodes,
                  particleContributing,
                  coordinates,
                  energy,
                  forces,
                  particleEnergy,
                  *virial,
                  particleVirial);
         break;
      case 102:
         ier = Compute< false, true,
                        true, false,
                        false, true,
                        true, false >(
                  modelCompute,
                  modelComputeArguments,
                  particleSpeciesCodes,
                  particleContributing,
                  coordinates,
                  energy,
                  forces,
                  particleEnergy,
                  *virial,
                  particleVirial);
         break;
      case 103:
         ier = Compute< false, true,
                        true, false,
                        false, true,
                        true, true >(
                  modelCompute,
                  modelComputeArguments,
                  particleSpeciesCodes,
                  particleContributing,
                  coordinates,
                  energy,
                  forces,
                  particleEnergy,
                  *virial,
                  particleVirial);
         break;
      case 104:
         ier = Compute< false, true,
                        true, false,
                        true, false,
                        false, false >(
                  modelCompute,
                  modelComputeArguments,
                  particleSpeciesCodes,
                  particleContributing,
                  coordinates,
                  energy,
                  forces,
                  particleEnergy,
                  *virial,
                  particleVirial);
         break;
      case 105:
         ier = Compute< false, true,
                        true, false,
                        true, false,
                        false, true >(
                  modelCompute,
                  modelComputeArguments,
                  particleSpeciesCodes,
                  particleContributing,
                  coordinates,
                  energy,
                  forces,
                  particleEnergy,
                  *virial,
                  particleVirial);
         break;
      case 106:
         ier = Compute< false, true,
                        true, false,
                        true, false,
                        true, false >(
                  modelCompute,
                  modelComputeArguments,
                  particleSpeciesCodes,
                  particleContributing,
                  coordinates,
                  energy,
                  forces,
                  particleEnergy,
                  *virial,
                  particleVirial);
         break;
      case 107:
         ier = Compute< false, true,
                        true, false,
                        true, false,
                        true, true >(
                  modelCompute,
                  modelComputeArguments,
                  particleSpeciesCodes,
                  particleContributing,
                  coordinates,
                  energy,
                  forces,
                  particleEnergy,
                  *virial,
                  particleVirial);
         break;
      case 108:
         ier = Compute< false, true,
                        true, false,
                        true, true,
                        false, false >(
                  modelCompute,
                  modelComputeArguments,
                  particleSpeciesCodes,
                  particleContributing,
                  coordinates,
                  energy,
                  forces,
                  particleEnergy,
                  *virial,
                  particleVirial);
         break;
      case 109:
         ier = Compute< false, true,
                        true, false,
                        true, true,
                        false, true >(
                  modelCompute,
                  modelComputeArguments,
                  particleSpeciesCodes,
                  particleContributing,
                  coordinates,
                  energy,
                  forces,
                  particleEnergy,
                  *virial,
                  particleVirial);
         break;
      case 110:
         ier = Compute< false, true,
                        true, false,
                        true, true,
                        true, false >(
                  modelCompute,
                  modelComputeArguments,
                  particleSpeciesCodes,
                  particleContributing,
                  coordinates,
                  energy,
                  forces,
                  particleEnergy,
                  *virial,
                  particleVirial);
         break;
      case 111:
         ier = Compute< false, true,
                        true, false,
                        true, true,
                        true, true >(
                  modelCompute,
                  modelComputeArguments,
                  particleSpeciesCodes,
                  particleContributing,
                  coordinates,
                  energy,
                  forces,
                  particleEnergy,
                  *virial,
                  particleVirial);
         break;
      case 112:
         ier = Compute< false, true,
                        true, true,
                        false, false,
                        false, false >(
                  modelCompute,
                  modelComputeArguments,
                  particleSpeciesCodes,
                  particleContributing,
                  coordinates,
                  energy,
                  forces,
                  particleEnergy,
                  *virial,
                  particleVirial);
         break;
      case 113:
         ier = Compute< false, true,
                        true, true,
                        false, false,
                        false, true >(
                  modelCompute,
                  modelComputeArguments,
                  particleSpeciesCodes,
                  particleContributing,
                  coordinates,
                  energy,
                  forces,
                  particleEnergy,
                  *virial,
                  particleVirial);
         break;
      case 114:
         ier = Compute< false, true,
                        true, true,
                        false, false,
                        true, false >(
                  modelCompute,
                  modelComputeArguments,
                  particleSpeciesCodes,
                  particleContributing,
                  coordinates,
                  energy,
                  forces,
                  particleEnergy,
                  *virial,
                  particleVirial);
         break;
      case 115:
         ier = Compute< false, true,
                        true, true,
                        false, false,
                        true, true >(
                  modelCompute,
                  modelComputeArguments,
                  particleSpeciesCodes,
                  particleContributing,
                  coordinates,
                  energy,
                  forces,
                  particleEnergy,
                  *virial,
                  particleVirial);
         break;
      case 116:
         ier = Compute< false, true,
                        true, true,
                        false, true,
                        false, false >(
                  modelCompute,
                  modelComputeArguments,
                  particleSpeciesCodes,
                  particleContributing,
                  coordinates,
                  energy,
                  forces,
                  particleEnergy,
                  *virial,
                  particleVirial);
         break;
      case 117:
         ier = Compute< false, true,
                        true, true,
                        false, true,
                        false, true >(
                  modelCompute,
                  modelComputeArguments,
                  particleSpeciesCodes,
                  particleContributing,
                  coordinates,
                  energy,
                  forces,
                  particleEnergy,
                  *virial,
                  particleVirial);
         break;
      case 118:
         ier = Compute< false, true,
                        true, true,
                        false, true,
                        true, false >(
                  modelCompute,
                  modelComputeArguments,
                  particleSpeciesCodes,
                  particleContributing,
                  coordinates,
                  energy,
                  forces,
                  particleEnergy,
                  *virial,
                  particleVirial);
         break;
      case 119:
         ier = Compute< false, true,
                        true, true,
                        false, true,
                        true, true >(
                  modelCompute,
                  modelComputeArguments,
                  particleSpeciesCodes,
                  particleContributing,
                  coordinates,
                  energy,
                  forces,
                  particleEnergy,
                  *virial,
                  particleVirial);
         break;
      case 120:
         ier = Compute< false, true,
                        true, true,
                        true, false,
                        false, false >(
                  modelCompute,
                  modelComputeArguments,
                  particleSpeciesCodes,
                  particleContributing,
                  coordinates,
                  energy,
                  forces,
                  particleEnergy,
                  *virial,
                  particleVirial);
         break;
      case 121:
         ier = Compute< false, true,
                        true, true,
                        true, false,
                        false, true >(
                  modelCompute,
                  modelComputeArguments,
                  particleSpeciesCodes,
                  particleContributing,
                  coordinates,
                  energy,
                  forces,
                  particleEnergy,
                  *virial,
                  particleVirial);
         break;
      case 122:
         ier = Compute< false, true,
                        true, true,
                        true, false,
                        true, false >(
                  modelCompute,
                  modelComputeArguments,
                  particleSpeciesCodes,
                  particleContributing,
                  coordinates,
                  energy,
                  forces,
                  particleEnergy,
                  *virial,
                  particleVirial);
         break;
      case 123:
         ier = Compute< false, true,
                        true, true,
                        true, false,
                        true, true >(
                  modelCompute,
                  modelComputeArguments,
                  particleSpeciesCodes,
                  particleContributing,
                  coordinates,
                  energy,
                  forces,
                  particleEnergy,
                  *virial,
                  particleVirial);
         break;
      case 124:
         ier = Compute< false, true,
                        true, true,
                        true, true,
                        false, false >(
                  modelCompute,
                  modelComputeArguments,
                  particleSpeciesCodes,
                  particleContributing,
                  coordinates,
                  energy,
                  forces,
                  particleEnergy,
                  *virial,
                  particleVirial);
         break;
      case 125:
         ier = Compute< false, true,
                        true, true,
                        true, true,
                        false, true >(
                  modelCompute,
                  modelComputeArguments,
                  particleSpeciesCodes,
                  particleContributing,
                  coordinates,
                  energy,
                  forces,
                  particleEnergy,
                  *virial,
                  particleVirial);
         break;
      case 126:
         ier = Compute< false, true,
                        true, true,
                        true, true,
                        true, false >(
                  modelCompute,
                  modelComputeArguments,
                  particleSpeciesCodes,
                  particleContributing,
                  coordinates,
                  energy,
                  forces,
                  particleEnergy,
                  *virial,
                  particleVirial);
         break;
      case 127:
         ier = Compute< false, true,
                        true, true,
                        true, true,
                        true, true >(
                  modelCompute,
                  modelComputeArguments,
                  particleSpeciesCodes,
                  particleContributing,
                  coordinates,
                  energy,
                  forces,
                  particleEnergy,
                  *virial,
                  particleVirial);
         break;
      case 128:
         ier = Compute< true, false,
                        false, false,
                        false, false,
                        false, false >(
                  modelCompute,
                  modelComputeArguments,
                  particleSpeciesCodes,
                  particleContributing,
                  coordinates,
                  energy,
                  forces,
                  particleEnergy,
                  *virial,
                  particleVirial);
         break;
      case 129:
         ier = Compute< true, false,
                        false, false,
                        false, false,
                        false, true >(
                  modelCompute,
                  modelComputeArguments,
                  particleSpeciesCodes,
                  particleContributing,
                  coordinates,
                  energy,
                  forces,
                  particleEnergy,
                  *virial,
                  particleVirial);
         break;
      case 130:
         ier = Compute< true, false,
                        false, false,
                        false, false,
                        true, false >(
                  modelCompute,
                  modelComputeArguments,
                  particleSpeciesCodes,
                  particleContributing,
                  coordinates,
                  energy,
                  forces,
                  particleEnergy,
                  *virial,
                  particleVirial);
         break;
      case 131:
         ier = Compute< true, false,
                        false, false,
                        false, false,
                        true, true >(
                  modelCompute,
                  modelComputeArguments,
                  particleSpeciesCodes,
                  particleContributing,
                  coordinates,
                  energy,
                  forces,
                  particleEnergy,
                  *virial,
                  particleVirial);
         break;
      case 132:
         ier = Compute< true, false,
                        false, false,
                        false, true,
                        false, false >(
                  modelCompute,
                  modelComputeArguments,
                  particleSpeciesCodes,
                  particleContributing,
                  coordinates,
                  energy,
                  forces,
                  particleEnergy,
                  *virial,
                  particleVirial);
         break;
      case 133:
         ier = Compute< true, false,
                        false, false,
                        false, true,
                        false, true >(
                  modelCompute,
                  modelComputeArguments,
                  particleSpeciesCodes,
                  particleContributing,
                  coordinates,
                  energy,
                  forces,
                  particleEnergy,
                  *virial,
                  particleVirial);
         break;
      case 134:
         ier = Compute< true, false,
                        false, false,
                        false, true,
                        true, false >(
                  modelCompute,
                  modelComputeArguments,
                  particleSpeciesCodes,
                  particleContributing,
                  coordinates,
                  energy,
                  forces,
                  particleEnergy,
                  *virial,
                  particleVirial);
         break;
      case 135:
         ier = Compute< true, false,
                        false, false,
                        false, true,
                        true, true >(
                  modelCompute,
                  modelComputeArguments,
                  particleSpeciesCodes,
                  particleContributing,
                  coordinates,
                  energy,
                  forces,
                  particleEnergy,
                  *virial,
                  particleVirial);
         break;
      case 136:
         ier = Compute< true, false,
                        false, false,
                        true, false,
                        false, false >(
                  modelCompute,
                  modelComputeArguments,
                  particleSpeciesCodes,
                  particleContributing,
                  coordinates,
                  energy,
                  forces,
                  particleEnergy,
                  *virial,
                  particleVirial);
         break;
      case 137:
         ier = Compute< true, false,
                        false, false,
                        true, false,
                        false, true >(
                  modelCompute,
                  modelComputeArguments,
                  particleSpeciesCodes,
                  particleContributing,
                  coordinates,
                  energy,
                  forces,
                  particleEnergy,
                  *virial,
                  particleVirial);
         break;
      case 138:
         ier = Compute< true, false,
                        false, false,
                        true, false,
                        true, false >(
                  modelCompute,
                  modelComputeArguments,
                  particleSpeciesCodes,
                  particleContributing,
                  coordinates,
                  energy,
                  forces,
                  particleEnergy,
                  *virial,
                  particleVirial);
         break;
      case 139:
         ier = Compute< true, false,
                        false, false,
                        true, false,
                        true, true >(
                  modelCompute,
                  modelComputeArguments,
                  particleSpeciesCodes,
                  particleContributing,
                  coordinates,
                  energy,
                  forces,
                  particleEnergy,
                  *virial,
                  particleVirial);
         break;
      case 140:
         ier = Compute< true, false,
                        false, false,
                        true, true,
                        false, false >(
                  modelCompute,
                  modelComputeArguments,
                  particleSpeciesCodes,
                  particleContributing,
                  coordinates,
                  energy,
                  forces,
                  particleEnergy,
                  *virial,
                  particleVirial);
         break;
      case 141:
         ier = Compute< true, false,
                        false, false,
                        true, true,
                        false, true >(
                  modelCompute,
                  modelComputeArguments,
                  particleSpeciesCodes,
                  particleContributing,
                  coordinates,
                  energy,
                  forces,
                  particleEnergy,
                  *virial,
                  particleVirial);
         break;
      case 142:
         ier = Compute< true, false,
                        false, false,
                        true, true,
                        true, false >(
                  modelCompute,
                  modelComputeArguments,
                  particleSpeciesCodes,
                  particleContributing,
                  coordinates,
                  energy,
                  forces,
                  particleEnergy,
                  *virial,
                  particleVirial);
         break;
      case 143:
         ier = Compute< true, false,
                        false, false,
                        true, true,
                        true, true >(
                  modelCompute,
                  modelComputeArguments,
                  particleSpeciesCodes,
                  particleContributing,
                  coordinates,
                  energy,
                  forces,
                  particleEnergy,
                  *virial,
                  particleVirial);
         break;
      case 144:
         ier = Compute< true, false,
                        false, true,
                        false, false,
                        false, false >(
                  modelCompute,
                  modelComputeArguments,
                  particleSpeciesCodes,
                  particleContributing,
                  coordinates,
                  energy,
                  forces,
                  particleEnergy,
                  *virial,
                  particleVirial);
         break;
      case 145:
         ier = Compute< true, false,
                        false, true,
                        false, false,
                        false, true >(
                  modelCompute,
                  modelComputeArguments,
                  particleSpeciesCodes,
                  particleContributing,
                  coordinates,
                  energy,
                  forces,
                  particleEnergy,
                  *virial,
                  particleVirial);
         break;
      case 146:
         ier = Compute< true, false,
                        false, true,
                        false, false,
                        true, false >(
                  modelCompute,
                  modelComputeArguments,
                  particleSpeciesCodes,
                  particleContributing,
                  coordinates,
                  energy,
                  forces,
                  particleEnergy,
                  *virial,
                  particleVirial);
         break;
      case 147:
         ier = Compute< true, false,
                        false, true,
                        false, false,
                        true, true >(
                  modelCompute,
                  modelComputeArguments,
                  particleSpeciesCodes,
                  particleContributing,
                  coordinates,
                  energy,
                  forces,
                  particleEnergy,
                  *virial,
                  particleVirial);
         break;
      case 148:
         ier = Compute< true, false,
                        false, true,
                        false, true,
                        false, false >(
                  modelCompute,
                  modelComputeArguments,
                  particleSpeciesCodes,
                  particleContributing,
                  coordinates,
                  energy,
                  forces,
                  particleEnergy,
                  *virial,
                  particleVirial);
         break;
      case 149:
         ier = Compute< true, false,
                        false, true,
                        false, true,
                        false, true >(
                  modelCompute,
                  modelComputeArguments,
                  particleSpeciesCodes,
                  particleContributing,
                  coordinates,
                  energy,
                  forces,
                  particleEnergy,
                  *virial,
                  particleVirial);
         break;
      case 150:
         ier = Compute< true, false,
                        false, true,
                        false, true,
                        true, false >(
                  modelCompute,
                  modelComputeArguments,
                  particleSpeciesCodes,
                  particleContributing,
                  coordinates,
                  energy,
                  forces,
                  particleEnergy,
                  *virial,
                  particleVirial);
         break;
      case 151:
         ier = Compute< true, false,
                        false, true,
                        false, true,
                        true, true >(
                  modelCompute,
                  modelComputeArguments,
                  particleSpeciesCodes,
                  particleContributing,
                  coordinates,
                  energy,
                  forces,
                  particleEnergy,
                  *virial,
                  particleVirial);
         break;
      case 152:
         ier = Compute< true, false,
                        false, true,
                        true, false,
                        false, false >(
                  modelCompute,
                  modelComputeArguments,
                  particleSpeciesCodes,
                  particleContributing,
                  coordinates,
                  energy,
                  forces,
                  particleEnergy,
                  *virial,
                  particleVirial);
         break;
      case 153:
         ier = Compute< true, false,
                        false, true,
                        true, false,
                        false, true >(
                  modelCompute,
                  modelComputeArguments,
                  particleSpeciesCodes,
                  particleContributing,
                  coordinates,
                  energy,
                  forces,
                  particleEnergy,
                  *virial,
                  particleVirial);
         break;
      case 154:
         ier = Compute< true, false,
                        false, true,
                        true, false,
                        true, false >(
                  modelCompute,
                  modelComputeArguments,
                  particleSpeciesCodes,
                  particleContributing,
                  coordinates,
                  energy,
                  forces,
                  particleEnergy,
                  *virial,
                  particleVirial);
         break;
      case 155:
         ier = Compute< true, false,
                        false, true,
                        true, false,
                        true, true >(
                  modelCompute,
                  modelComputeArguments,
                  particleSpeciesCodes,
                  particleContributing,
                  coordinates,
                  energy,
                  forces,
                  particleEnergy,
                  *virial,
                  particleVirial);
         break;
      case 156:
         ier = Compute< true, false,
                        false, true,
                        true, true,
                        false, false >(
                  modelCompute,
                  modelComputeArguments,
                  particleSpeciesCodes,
                  particleContributing,
                  coordinates,
                  energy,
                  forces,
                  particleEnergy,
                  *virial,
                  particleVirial);
         break;
      case 157:
         ier = Compute< true, false,
                        false, true,
                        true, true,
                        false, true >(
                  modelCompute,
                  modelComputeArguments,
                  particleSpeciesCodes,
                  particleContributing,
                  coordinates,
                  energy,
                  forces,
                  particleEnergy,
                  *virial,
                  particleVirial);
         break;
      case 158:
         ier = Compute< true, false,
                        false, true,
                        true, true,
                        true, false >(
                  modelCompute,
                  modelComputeArguments,
                  particleSpeciesCodes,
                  particleContributing,
                  coordinates,
                  energy,
                  forces,
                  particleEnergy,
                  *virial,
                  particleVirial);
         break;
      case 159:
         ier = Compute< true, false,
                        false, true,
                        true, true,
                        true, true >(
                  modelCompute,
                  modelComputeArguments,
                  particleSpeciesCodes,
                  particleContributing,
                  coordinates,
                  energy,
                  forces,
                  particleEnergy,
                  *virial,
                  particleVirial);
         break;
      case 160:
         ier = Compute< true, false,
                        true, false,
                        false, false,
                        false, false >(
                  modelCompute,
                  modelComputeArguments,
                  particleSpeciesCodes,
                  particleContributing,
                  coordinates,
                  energy,
                  forces,
                  particleEnergy,
                  *virial,
                  particleVirial);
         break;
      case 161:
         ier = Compute< true, false,
                        true, false,
                        false, false,
                        false, true >(
                  modelCompute,
                  modelComputeArguments,
                  particleSpeciesCodes,
                  particleContributing,
                  coordinates,
                  energy,
                  forces,
                  particleEnergy,
                  *virial,
                  particleVirial);
         break;
      case 162:
         ier = Compute< true, false,
                        true, false,
                        false, false,
                        true, false >(
                  modelCompute,
                  modelComputeArguments,
                  particleSpeciesCodes,
                  particleContributing,
                  coordinates,
                  energy,
                  forces,
                  particleEnergy,
                  *virial,
                  particleVirial);
         break;
      case 163:
         ier = Compute< true, false,
                        true, false,
                        false, false,
                        true, true >(
                  modelCompute,
                  modelComputeArguments,
                  particleSpeciesCodes,
                  particleContributing,
                  coordinates,
                  energy,
                  forces,
                  particleEnergy,
                  *virial,
                  particleVirial);
         break;
      case 164:
         ier = Compute< true, false,
                        true, false,
                        false, true,
                        false, false >(
                  modelCompute,
                  modelComputeArguments,
                  particleSpeciesCodes,
                  particleContributing,
                  coordinates,
                  energy,
                  forces,
                  particleEnergy,
                  *virial,
                  particleVirial);
         break;
      case 165:
         ier = Compute< true, false,
                        true, false,
                        false, true,
                        false, true >(
                  modelCompute,
                  modelComputeArguments,
                  particleSpeciesCodes,
                  particleContributing,
                  coordinates,
                  energy,
                  forces,
                  particleEnergy,
                  *virial,
                  particleVirial);
         break;
      case 166:
         ier = Compute< true, false,
                        true, false,
                        false, true,
                        true, false >(
                  modelCompute,
                  modelComputeArguments,
                  particleSpeciesCodes,
                  particleContributing,
                  coordinates,
                  energy,
                  forces,
                  particleEnergy,
                  *virial,
                  particleVirial);
         break;
      case 167:
         ier = Compute< true, false,
                        true, false,
                        false, true,
                        true, true >(
                  modelCompute,
                  modelComputeArguments,
                  particleSpeciesCodes,
                  particleContributing,
                  coordinates,
                  energy,
                  forces,
                  particleEnergy,
                  *virial,
                  particleVirial);
         break;
      case 168:
         ier = Compute< true, false,
                        true, false,
                        true, false,
                        false, false >(
                  modelCompute,
                  modelComputeArguments,
                  particleSpeciesCodes,
                  particleContributing,
                  coordinates,
                  energy,
                  forces,
                  particleEnergy,
                  *virial,
                  particleVirial);
         break;
      case 169:
         ier = Compute< true, false,
                        true, false,
                        true, false,
                        false, true >(
                  modelCompute,
                  modelComputeArguments,
                  particleSpeciesCodes,
                  particleContributing,
                  coordinates,
                  energy,
                  forces,
                  particleEnergy,
                  *virial,
                  particleVirial);
         break;
      case 170:
         ier = Compute< true, false,
                        true, false,
                        true, false,
                        true, false >(
                  modelCompute,
                  modelComputeArguments,
                  particleSpeciesCodes,
                  particleContributing,
                  coordinates,
                  energy,
                  forces,
                  particleEnergy,
                  *virial,
                  particleVirial);
         break;
      case 171:
         ier = Compute< true, false,
                        true, false,
                        true, false,
                        true, true >(
                  modelCompute,
                  modelComputeArguments,
                  particleSpeciesCodes,
                  particleContributing,
                  coordinates,
                  energy,
                  forces,
                  particleEnergy,
                  *virial,
                  particleVirial);
         break;
      case 172:
         ier = Compute< true, false,
                        true, false,
                        true, true,
                        false, false >(
                  modelCompute,
                  modelComputeArguments,
                  particleSpeciesCodes,
                  particleContributing,
                  coordinates,
                  energy,
                  forces,
                  particleEnergy,
                  *virial,
                  particleVirial);
         break;
      case 173:
         ier = Compute< true, false,
                        true, false,
                        true, true,
                        false, true >(
                  modelCompute,
                  modelComputeArguments,
                  particleSpeciesCodes,
                  particleContributing,
                  coordinates,
                  energy,
                  forces,
                  particleEnergy,
                  *virial,
                  particleVirial);
         break;
      case 174:
         ier = Compute< true, false,
                        true, false,
                        true, true,
                        true, false >(
                  modelCompute,
                  modelComputeArguments,
                  particleSpeciesCodes,
                  particleContributing,
                  coordinates,
                  energy,
                  forces,
                  particleEnergy,
                  *virial,
                  particleVirial);
         break;
      case 175:
         ier = Compute< true, false,
                        true, false,
                        true, true,
                        true, true >(
                  modelCompute,
                  modelComputeArguments,
                  particleSpeciesCodes,
                  particleContributing,
                  coordinates,
                  energy,
                  forces,
                  particleEnergy,
                  *virial,
                  particleVirial);
         break;
      case 176:
         ier = Compute< true, false,
                        true, true,
                        false, false,
                        false, false >(
                  modelCompute,
                  modelComputeArguments,
                  particleSpeciesCodes,
                  particleContributing,
                  coordinates,
                  energy,
                  forces,
                  particleEnergy,
                  *virial,
                  particleVirial);
         break;
      case 177:
         ier = Compute< true, false,
                        true, true,
                        false, false,
                        false, true >(
                  modelCompute,
                  modelComputeArguments,
                  particleSpeciesCodes,
                  particleContributing,
                  coordinates,
                  energy,
                  forces,
                  particleEnergy,
                  *virial,
                  particleVirial);
         break;
      case 178:
         ier = Compute< true, false,
                        true, true,
                        false, false,
                        true, false >(
                  modelCompute,
                  modelComputeArguments,
                  particleSpeciesCodes,
                  particleContributing,
                  coordinates,
                  energy,
                  forces,
                  particleEnergy,
                  *virial,
                  particleVirial);
         break;
      case 179:
         ier = Compute< true, false,
                        true, true,
                        false, false,
                        true, true >(
                  modelCompute,
                  modelComputeArguments,
                  particleSpeciesCodes,
                  particleContributing,
                  coordinates,
                  energy,
                  forces,
                  particleEnergy,
                  *virial,
                  particleVirial);
         break;
      case 180:
         ier = Compute< true, false,
                        true, true,
                        false, true,
                        false, false >(
                  modelCompute,
                  modelComputeArguments,
                  particleSpeciesCodes,
                  particleContributing,
                  coordinates,
                  energy,
                  forces,
                  particleEnergy,
                  *virial,
                  particleVirial);
         break;
      case 181:
         ier = Compute< true, false,
                        true, true,
                        false, true,
                        false, true >(
                  modelCompute,
                  modelComputeArguments,
                  particleSpeciesCodes,
                  particleContributing,
                  coordinates,
                  energy,
                  forces,
                  particleEnergy,
                  *virial,
                  particleVirial);
         break;
      case 182:
         ier = Compute< true, false,
                        true, true,
                        false, true,
                        true, false >(
                  modelCompute,
                  modelComputeArguments,
                  particleSpeciesCodes,
                  particleContributing,
                  coordinates,
                  energy,
                  forces,
                  particleEnergy,
                  *virial,
                  particleVirial);
         break;
      case 183:
         ier = Compute< true, false,
                        true, true,
                        false, true,
                        true, true >(
                  modelCompute,
                  modelComputeArguments,
                  particleSpeciesCodes,
                  particleContributing,
                  coordinates,
                  energy,
                  forces,
                  particleEnergy,
                  *virial,
                  particleVirial);
         break;
      case 184:
         ier = Compute< true, false,
                        true, true,
                        true, false,
                        false, false >(
                  modelCompute,
                  modelComputeArguments,
                  particleSpeciesCodes,
                  particleContributing,
                  coordinates,
                  energy,
                  forces,
                  particleEnergy,
                  *virial,
                  particleVirial);
         break;
      case 185:
         ier = Compute< true, false,
                        true, true,
                        true, false,
                        false, true >(
                  modelCompute,
                  modelComputeArguments,
                  particleSpeciesCodes,
                  particleContributing,
                  coordinates,
                  energy,
                  forces,
                  particleEnergy,
                  *virial,
                  particleVirial);
         break;
      case 186:
         ier = Compute< true, false,
                        true, true,
                        true, false,
                        true, false >(
                  modelCompute,
                  modelComputeArguments,
                  particleSpeciesCodes,
                  particleContributing,
                  coordinates,
                  energy,
                  forces,
                  particleEnergy,
                  *virial,
                  particleVirial);
         break;
      case 187:
         ier = Compute< true, false,
                        true, true,
                        true, false,
                        true, true >(
                  modelCompute,
                  modelComputeArguments,
                  particleSpeciesCodes,
                  particleContributing,
                  coordinates,
                  energy,
                  forces,
                  particleEnergy,
                  *virial,
                  particleVirial);
         break;
      case 188:
         ier = Compute< true, false,
                        true, true,
                        true, true,
                        false, false >(
                  modelCompute,
                  modelComputeArguments,
                  particleSpeciesCodes,
                  particleContributing,
                  coordinates,
                  energy,
                  forces,
                  particleEnergy,
                  *virial,
                  particleVirial);
         break;
      case 189:
         ier = Compute< true, false,
                        true, true,
                        true, true,
                        false, true >(
                  modelCompute,
                  modelComputeArguments,
                  particleSpeciesCodes,
                  particleContributing,
                  coordinates,
                  energy,
                  forces,
                  particleEnergy,
                  *virial,
                  particleVirial);
         break;
      case 190:
         ier = Compute< true, false,
                        true, true,
                        true, true,
                        true, false >(
                  modelCompute,
                  modelComputeArguments,
                  particleSpeciesCodes,
                  particleContributing,
                  coordinates,
                  energy,
                  forces,
                  particleEnergy,
                  *virial,
                  particleVirial);
         break;
      case 191:
         ier = Compute< true, false,
                        true, true,
                        true, true,
                        true, true >(
                  modelCompute,
                  modelComputeArguments,
                  particleSpeciesCodes,
                  particleContributing,
                  coordinates,
                  energy,
                  forces,
                  particleEnergy,
                  *virial,
                  particleVirial);
         break;
      case 192:
         ier = Compute< true, true,
                        false, false,
                        false, false,
                        false, false >(
                  modelCompute,
                  modelComputeArguments,
                  particleSpeciesCodes,
                  particleContributing,
                  coordinates,
                  energy,
                  forces,
                  particleEnergy,
                  *virial,
                  particleVirial);
         break;
      case 193:
         ier = Compute< true, true,
                        false, false,
                        false, false,
                        false, true >(
                  modelCompute,
                  modelComputeArguments,
                  particleSpeciesCodes,
                  particleContributing,
                  coordinates,
                  energy,
                  forces,
                  particleEnergy,
                  *virial,
                  particleVirial);
         break;
      case 194:
         ier = Compute< true, true,
                        false, false,
                        false, false,
                        true, false >(
                  modelCompute,
                  modelComputeArguments,
                  particleSpeciesCodes,
                  particleContributing,
                  coordinates,
                  energy,
                  forces,
                  particleEnergy,
                  *virial,
                  particleVirial);
         break;
      case 195:
         ier = Compute< true, true,
                        false, false,
                        false, false,
                        true, true >(
                  modelCompute,
                  modelComputeArguments,
                  particleSpeciesCodes,
                  particleContributing,
                  coordinates,
                  energy,
                  forces,
                  particleEnergy,
                  *virial,
                  particleVirial);
         break;
      case 196:
         ier = Compute< true, true,
                        false, false,
                        false, true,
                        false, false >(
                  modelCompute,
                  modelComputeArguments,
                  particleSpeciesCodes,
                  particleContributing,
                  coordinates,
                  energy,
                  forces,
                  particleEnergy,
                  *virial,
                  particleVirial);
         break;
      case 197:
         ier = Compute< true, true,
                        false, false,
                        false, true,
                        false, true >(
                  modelCompute,
                  modelComputeArguments,
                  particleSpeciesCodes,
                  particleContributing,
                  coordinates,
                  energy,
                  forces,
                  particleEnergy,
                  *virial,
                  particleVirial);
         break;
      case 198:
         ier = Compute< true, true,
                        false, false,
                        false, true,
                        true, false >(
                  modelCompute,
                  modelComputeArguments,
                  particleSpeciesCodes,
                  particleContributing,
                  coordinates,
                  energy,
                  forces,
                  particleEnergy,
                  *virial,
                  particleVirial);
         break;
      case 199:
         ier = Compute< true, true,
                        false, false,
                        false, true,
                        true, true >(
                  modelCompute,
                  modelComputeArguments,
                  particleSpeciesCodes,
                  particleContributing,
                  coordinates,
                  energy,
                  forces,
                  particleEnergy,
                  *virial,
                  particleVirial);
         break;
      case 200:
         ier = Compute< true, true,
                        false, false,
                        true, false,
                        false, false >(
                  modelCompute,
                  modelComputeArguments,
                  particleSpeciesCodes,
                  particleContributing,
                  coordinates,
                  energy,
                  forces,
                  particleEnergy,
                  *virial,
                  particleVirial);
         break;
      case 201:
         ier = Compute< true, true,
                        false, false,
                        true, false,
                        false, true >(
                  modelCompute,
                  modelComputeArguments,
                  particleSpeciesCodes,
                  particleContributing,
                  coordinates,
                  energy,
                  forces,
                  particleEnergy,
                  *virial,
                  particleVirial);
         break;
      case 202:
         ier = Compute< true, true,
                        false, false,
                        true, false,
                        true, false >(
                  modelCompute,
                  modelComputeArguments,
                  particleSpeciesCodes,
                  particleContributing,
                  coordinates,
                  energy,
                  forces,
                  particleEnergy,
                  *virial,
                  particleVirial);
         break;
      case 203:
         ier = Compute< true, true,
                        false, false,
                        true, false,
                        true, true >(
                  modelCompute,
                  modelComputeArguments,
                  particleSpeciesCodes,
                  particleContributing,
                  coordinates,
                  energy,
                  forces,
                  particleEnergy,
                  *virial,
                  particleVirial);
         break;
      case 204:
         ier = Compute< true, true,
                        false, false,
                        true, true,
                        false, false >(
                  modelCompute,
                  modelComputeArguments,
                  particleSpeciesCodes,
                  particleContributing,
                  coordinates,
                  energy,
                  forces,
                  particleEnergy,
                  *virial,
                  particleVirial);
         break;
      case 205:
         ier = Compute< true, true,
                        false, false,
                        true, true,
                        false, true >(
                  modelCompute,
                  modelComputeArguments,
                  particleSpeciesCodes,
                  particleContributing,
                  coordinates,
                  energy,
                  forces,
                  particleEnergy,
                  *virial,
                  particleVirial);
         break;
      case 206:
         ier = Compute< true, true,
                        false, false,
                        true, true,
                        true, false >(
                  modelCompute,
                  modelComputeArguments,
                  particleSpeciesCodes,
                  particleContributing,
                  coordinates,
                  energy,
                  forces,
                  particleEnergy,
                  *virial,
                  particleVirial);
         break;
      case 207:
         ier = Compute< true, true,
                        false, false,
                        true, true,
                        true, true >(
                  modelCompute,
                  modelComputeArguments,
                  particleSpeciesCodes,
                  particleContributing,
                  coordinates,
                  energy,
                  forces,
                  particleEnergy,
                  *virial,
                  particleVirial);
         break;
      case 208:
         ier = Compute< true, true,
                        false, true,
                        false, false,
                        false, false >(
                  modelCompute,
                  modelComputeArguments,
                  particleSpeciesCodes,
                  particleContributing,
                  coordinates,
                  energy,
                  forces,
                  particleEnergy,
                  *virial,
                  particleVirial);
         break;
      case 209:
         ier = Compute< true, true,
                        false, true,
                        false, false,
                        false, true >(
                  modelCompute,
                  modelComputeArguments,
                  particleSpeciesCodes,
                  particleContributing,
                  coordinates,
                  energy,
                  forces,
                  particleEnergy,
                  *virial,
                  particleVirial);
         break;
      case 210:
         ier = Compute< true, true,
                        false, true,
                        false, false,
                        true, false >(
                  modelCompute,
                  modelComputeArguments,
                  particleSpeciesCodes,
                  particleContributing,
                  coordinates,
                  energy,
                  forces,
                  particleEnergy,
                  *virial,
                  particleVirial);
         break;
      case 211:
         ier = Compute< true, true,
                        false, true,
                        false, false,
                        true, true >(
                  modelCompute,
                  modelComputeArguments,
                  particleSpeciesCodes,
                  particleContributing,
                  coordinates,
                  energy,
                  forces,
                  particleEnergy,
                  *virial,
                  particleVirial);
         break;
      case 212:
         ier = Compute< true, true,
                        false, true,
                        false, true,
                        false, false >(
                  modelCompute,
                  modelComputeArguments,
                  particleSpeciesCodes,
                  particleContributing,
                  coordinates,
                  energy,
                  forces,
                  particleEnergy,
                  *virial,
                  particleVirial);
         break;
      case 213:
         ier = Compute< true, true,
                        false, true,
                        false, true,
                        false, true >(
                  modelCompute,
                  modelComputeArguments,
                  particleSpeciesCodes,
                  particleContributing,
                  coordinates,
                  energy,
                  forces,
                  particleEnergy,
                  *virial,
                  particleVirial);
         break;
      case 214:
         ier = Compute< true, true,
                        false, true,
                        false, true,
                        true, false >(
                  modelCompute,
                  modelComputeArguments,
                  particleSpeciesCodes,
                  particleContributing,
                  coordinates,
                  energy,
                  forces,
                  particleEnergy,
                  *virial,
                  particleVirial);
         break;
      case 215:
         ier = Compute< true, true,
                        false, true,
                        false, true,
                        true, true >(
                  modelCompute,
                  modelComputeArguments,
                  particleSpeciesCodes,
                  particleContributing,
                  coordinates,
                  energy,
                  forces,
                  particleEnergy,
                  *virial,
                  particleVirial);
         break;
      case 216:
         ier = Compute< true, true,
                        false, true,
                        true, false,
                        false, false >(
                  modelCompute,
                  modelComputeArguments,
                  particleSpeciesCodes,
                  particleContributing,
                  coordinates,
                  energy,
                  forces,
                  particleEnergy,
                  *virial,
                  particleVirial);
         break;
      case 217:
         ier = Compute< true, true,
                        false, true,
                        true, false,
                        false, true >(
                  modelCompute,
                  modelComputeArguments,
                  particleSpeciesCodes,
                  particleContributing,
                  coordinates,
                  energy,
                  forces,
                  particleEnergy,
                  *virial,
                  particleVirial);
         break;
      case 218:
         ier = Compute< true, true,
                        false, true,
                        true, false,
                        true, false >(
                  modelCompute,
                  modelComputeArguments,
                  particleSpeciesCodes,
                  particleContributing,
                  coordinates,
                  energy,
                  forces,
                  particleEnergy,
                  *virial,
                  particleVirial);
         break;
      case 219:
         ier = Compute< true, true,
                        false, true,
                        true, false,
                        true, true >(
                  modelCompute,
                  modelComputeArguments,
                  particleSpeciesCodes,
                  particleContributing,
                  coordinates,
                  energy,
                  forces,
                  particleEnergy,
                  *virial,
                  particleVirial);
         break;
      case 220:
         ier = Compute< true, true,
                        false, true,
                        true, true,
                        false, false >(
                  modelCompute,
                  modelComputeArguments,
                  particleSpeciesCodes,
                  particleContributing,
                  coordinates,
                  energy,
                  forces,
                  particleEnergy,
                  *virial,
                  particleVirial);
         break;
      case 221:
         ier = Compute< true, true,
                        false, true,
                        true, true,
                        false, true >(
                  modelCompute,
                  modelComputeArguments,
                  particleSpeciesCodes,
                  particleContributing,
                  coordinates,
                  energy,
                  forces,
                  particleEnergy,
                  *virial,
                  particleVirial);
         break;
      case 222:
         ier = Compute< true, true,
                        false, true,
                        true, true,
                        true, false >(
                  modelCompute,
                  modelComputeArguments,
                  particleSpeciesCodes,
                  particleContributing,
                  coordinates,
                  energy,
                  forces,
                  particleEnergy,
                  *virial,
                  particleVirial);
         break;
      case 223:
         ier = Compute< true, true,
                        false, true,
                        true, true,
                        true, true >(
                  modelCompute,
                  modelComputeArguments,
                  particleSpeciesCodes,
                  particleContributing,
                  coordinates,
                  energy,
                  forces,
                  particleEnergy,
                  *virial,
                  particleVirial);
         break;
      case 224:
         ier = Compute< true, true,
                        true, false,
                        false, false,
                        false, false >(
                  modelCompute,
                  modelComputeArguments,
                  particleSpeciesCodes,
                  particleContributing,
                  coordinates,
                  energy,
                  forces,
                  particleEnergy,
                  *virial,
                  particleVirial);
         break;
      case 225:
         ier = Compute< true, true,
                        true, false,
                        false, false,
                        false, true >(
                  modelCompute,
                  modelComputeArguments,
                  particleSpeciesCodes,
                  particleContributing,
                  coordinates,
                  energy,
                  forces,
                  particleEnergy,
                  *virial,
                  particleVirial);
         break;
      case 226:
         ier = Compute< true, true,
                        true, false,
                        false, false,
                        true, false >(
                  modelCompute,
                  modelComputeArguments,
                  particleSpeciesCodes,
                  particleContributing,
                  coordinates,
                  energy,
                  forces,
                  particleEnergy,
                  *virial,
                  particleVirial);
         break;
      case 227:
         ier = Compute< true, true,
                        true, false,
                        false, false,
                        true, true >(
                  modelCompute,
                  modelComputeArguments,
                  particleSpeciesCodes,
                  particleContributing,
                  coordinates,
                  energy,
                  forces,
                  particleEnergy,
                  *virial,
                  particleVirial);
         break;
      case 228:
         ier = Compute< true, true,
                        true, false,
                        false, true,
                        false, false >(
                  modelCompute,
                  modelComputeArguments,
                  particleSpeciesCodes,
                  particleContributing,
                  coordinates,
                  energy,
                  forces,
                  particleEnergy,
                  *virial,
                  particleVirial);
         break;
      case 229:
         ier = Compute< true, true,
                        true, false,
                        false, true,
                        false, true >(
                  modelCompute,
                  modelComputeArguments,
                  particleSpeciesCodes,
                  particleContributing,
                  coordinates,
                  energy,
                  forces,
                  particleEnergy,
                  *virial,
                  particleVirial);
         break;
      case 230:
         ier = Compute< true, true,
                        true, false,
                        false, true,
                        true, false >(
                  modelCompute,
                  modelComputeArguments,
                  particleSpeciesCodes,
                  particleContributing,
                  coordinates,
                  energy,
                  forces,
                  particleEnergy,
                  *virial,
                  particleVirial);
         break;
      case 231:
         ier = Compute< true, true,
                        true, false,
                        false, true,
                        true, true >(
                  modelCompute,
                  modelComputeArguments,
                  particleSpeciesCodes,
                  particleContributing,
                  coordinates,
                  energy,
                  forces,
                  particleEnergy,
                  *virial,
                  particleVirial);
         break;
      case 232:
         ier = Compute< true, true,
                        true, false,
                        true, false,
                        false, false >(
                  modelCompute,
                  modelComputeArguments,
                  particleSpeciesCodes,
                  particleContributing,
                  coordinates,
                  energy,
                  forces,
                  particleEnergy,
                  *virial,
                  particleVirial);
         break;
      case 233:
         ier = Compute< true, true,
                        true, false,
                        true, false,
                        false, true >(
                  modelCompute,
                  modelComputeArguments,
                  particleSpeciesCodes,
                  particleContributing,
                  coordinates,
                  energy,
                  forces,
                  particleEnergy,
                  *virial,
                  particleVirial);
         break;
      case 234:
         ier = Compute< true, true,
                        true, false,
                        true, false,
                        true, false >(
                  modelCompute,
                  modelComputeArguments,
                  particleSpeciesCodes,
                  particleContributing,
                  coordinates,
                  energy,
                  forces,
                  particleEnergy,
                  *virial,
                  particleVirial);
         break;
      case 235:
         ier = Compute< true, true,
                        true, false,
                        true, false,
                        true, true >(
                  modelCompute,
                  modelComputeArguments,
                  particleSpeciesCodes,
                  particleContributing,
                  coordinates,
                  energy,
                  forces,
                  particleEnergy,
                  *virial,
                  particleVirial);
         break;
      case 236:
         ier = Compute< true, true,
                        true, false,
                        true, true,
                        false, false >(
                  modelCompute,
                  modelComputeArguments,
                  particleSpeciesCodes,
                  particleContributing,
                  coordinates,
                  energy,
                  forces,
                  particleEnergy,
                  *virial,
                  particleVirial);
         break;
      case 237:
         ier = Compute< true, true,
                        true, false,
                        true, true,
                        false, true >(
                  modelCompute,
                  modelComputeArguments,
                  particleSpeciesCodes,
                  particleContributing,
                  coordinates,
                  energy,
                  forces,
                  particleEnergy,
                  *virial,
                  particleVirial);
         break;
      case 238:
         ier = Compute< true, true,
                        true, false,
                        true, true,
                        true, false >(
                  modelCompute,
                  modelComputeArguments,
                  particleSpeciesCodes,
                  particleContributing,
                  coordinates,
                  energy,
                  forces,
                  particleEnergy,
                  *virial,
                  particleVirial);
         break;
      case 239:
         ier = Compute< true, true,
                        true, false,
                        true, true,
                        true, true >(
                  modelCompute,
                  modelComputeArguments,
                  particleSpeciesCodes,
                  particleContributing,
                  coordinates,
                  energy,
                  forces,
                  particleEnergy,
                  *virial,
                  particleVirial);
         break;
      case 240:
         ier = Compute< true, true,
                        true, true,
                        false, false,
                        false, false >(
                  modelCompute,
                  modelComputeArguments,
                  particleSpeciesCodes,
                  particleContributing,
                  coordinates,
                  energy,
                  forces,
                  particleEnergy,
                  *virial,
                  particleVirial);
         break;
      case 241:
         ier = Compute< true, true,
                        true, true,
                        false, false,
                        false, true >(
                  modelCompute,
                  modelComputeArguments,
                  particleSpeciesCodes,
                  particleContributing,
                  coordinates,
                  energy,
                  forces,
                  particleEnergy,
                  *virial,
                  particleVirial);
         break;
      case 242:
         ier = Compute< true, true,
                        true, true,
                        false, false,
                        true, false >(
                  modelCompute,
                  modelComputeArguments,
                  particleSpeciesCodes,
                  particleContributing,
                  coordinates,
                  energy,
                  forces,
                  particleEnergy,
                  *virial,
                  particleVirial);
         break;
      case 243:
         ier = Compute< true, true,
                        true, true,
                        false, false,
                        true, true >(
                  modelCompute,
                  modelComputeArguments,
                  particleSpeciesCodes,
                  particleContributing,
                  coordinates,
                  energy,
                  forces,
                  particleEnergy,
                  *virial,
                  particleVirial);
         break;
      case 244:
         ier = Compute< true, true,
                        true, true,
                        false, true,
                        false, false >(
                  modelCompute,
                  modelComputeArguments,
                  particleSpeciesCodes,
                  particleContributing,
                  coordinates,
                  energy,
                  forces,
                  particleEnergy,
                  *virial,
                  particleVirial);
         break;
      case 245:
         ier = Compute< true, true,
                        true, true,
                        false, true,
                        false, true >(
                  modelCompute,
                  modelComputeArguments,
                  particleSpeciesCodes,
                  particleContributing,
                  coordinates,
                  energy,
                  forces,
                  particleEnergy,
                  *virial,
                  particleVirial);
         break;
      case 246:
         ier = Compute< true, true,
                        true, true,
                        false, true,
                        true, false >(
                  modelCompute,
                  modelComputeArguments,
                  particleSpeciesCodes,
                  particleContributing,
                  coordinates,
                  energy,
                  forces,
                  particleEnergy,
                  *virial,
                  particleVirial);
         break;
      case 247:
         ier = Compute< true, true,
                        true, true,
                        false, true,
                        true, true >(
                  modelCompute,
                  modelComputeArguments,
                  particleSpeciesCodes,
                  particleContributing,
                  coordinates,
                  energy,
                  forces,
                  particleEnergy,
                  *virial,
                  particleVirial);
         break;
      case 248:
         ier = Compute< true, true,
                        true, true,
                        true, false,
                        false, false >(
                  modelCompute,
                  modelComputeArguments,
                  particleSpeciesCodes,
                  particleContributing,
                  coordinates,
                  energy,
                  forces,
                  particleEnergy,
                  *virial,
                  particleVirial);
         break;
      case 249:
         ier = Compute< true, true,
                        true, true,
                        true, false,
                        false, true >(
                  modelCompute,
                  modelComputeArguments,
                  particleSpeciesCodes,
                  particleContributing,
                  coordinates,
                  energy,
                  forces,
                  particleEnergy,
                  *virial,
                  particleVirial);
         break;
      case 250:
         ier = Compute< true, true,
                        true, true,
                        true, false,
                        true, false >(
                  modelCompute,
                  modelComputeArguments,
                  particleSpeciesCodes,
                  particleContributing,
                  coordinates,
                  energy,
                  forces,
                  particleEnergy,
                  *virial,
                  particleVirial);
         break;
      case 251:
         ier = Compute< true, true,
                        true, true,
                        true, false,
                        true, true >(
                  modelCompute,
                  modelComputeArguments,
                  particleSpeciesCodes,
                  particleContributing,
                  coordinates,
                  energy,
                  forces,
                  particleEnergy,
                  *virial,
                  particleVirial);
         break;
      case 252:
         ier = Compute< true, true,
                        true, true,
                        true, true,
                        false, false >(
                  modelCompute,
                  modelComputeArguments,
                  particleSpeciesCodes,
                  particleContributing,
                  coordinates,
                  energy,
                  forces,
                  particleEnergy,
                  *virial,
                  particleVirial);
         break;
      case 253:
         ier = Compute< true, true,
                        true, true,
                        true, true,
                        false, true >(
                  modelCompute,
                  modelComputeArguments,
                  particleSpeciesCodes,
                  particleContributing,
                  coordinates,
                  energy,
                  forces,
                  particleEnergy,
                  *virial,
                  particleVirial);
         break;
      case 254:
         ier = Compute< true, true,
                        true, true,
                        true, true,
                        true, false >(
                  modelCompute,
                  modelComputeArguments,
                  particleSpeciesCodes,
                  particleContributing,
                  coordinates,
                  energy,
                  forces,
                  particleEnergy,
                  *virial,
                  particleVirial);
         break;
      case 255:
         ier = Compute< true, true,
                        true, true,
                        true, true,
                        true, true >(
                  modelCompute,
                  modelComputeArguments,
                  particleSpeciesCodes,
                  particleContributing,
                  coordinates,
                  energy,
                  forces,
                  particleEnergy,
                  *virial,
                  particleVirial);
         break;
      default:
         std::cout << "Unknown compute function index" << std::endl;
         ier = true;
         break;
   }
