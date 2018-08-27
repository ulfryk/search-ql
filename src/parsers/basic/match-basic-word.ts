import { Maybe } from 'monet';

import { SyntaxConfig } from '../../config';

export const matchBasicWord = (input: string, { binaryOperators, restrictedSigns }: SyntaxConfig) =>
  Maybe.fromNull(input.match(restrictedSigns))
    .filter(([match]) => !binaryOperators.includes(match));
