import { Maybe } from 'monet';

import { ParserConfig } from '../../config';

export const matchTerm =
  (input: string, { binaryOperators, restrictedSigns }: ParserConfig): Maybe<RegExpMatchArray> =>
    Maybe.fromNull(input.match(restrictedSigns))
      .filter(([match]) => !binaryOperators.includes(match));
