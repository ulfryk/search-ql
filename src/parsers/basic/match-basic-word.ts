import { Maybe } from 'monet';

import { SyntaxConfig } from '../../config';

export const matchBasicWord = (input: string, { AND, OR, restrictedSigns }: SyntaxConfig) =>
  Maybe.fromNull(input.match(restrictedSigns))
    .filter(([match]) => ![AND, OR].includes(match));
