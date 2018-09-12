import { Maybe } from 'monet';

import { ParserConfig } from '../../config';

export const matchBasicWord = (input: string, { binaryOperators, restrictedSigns }: ParserConfig) =>
  Maybe.fromNull(input.match(restrictedSigns))
    .filter(([match]) => !binaryOperators.includes(match));
