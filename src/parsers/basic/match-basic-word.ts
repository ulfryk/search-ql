import { Maybe } from 'monet';

import { AND, OR } from '../../syntax-config';
import { restrictedSigns } from '../common';

export const matchBasicWord = (input: string) =>
  Maybe.fromNull(input.match(restrictedSigns))
    .filter(([match]) => ![AND, OR].includes(match));
