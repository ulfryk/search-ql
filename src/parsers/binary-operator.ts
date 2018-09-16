import * as P from 'parsimmon';

import { ParserConfig } from '../config';

// FIXME: NOT should move out maybe ?
export const binaryOperator = ({ binaryOperators, NOT }: ParserConfig) =>
  P.alt(...[...binaryOperators, ...NOT].map(operator => P.string(operator)));
