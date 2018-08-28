import * as P from 'parsimmon';

import { SyntaxConfig } from '../config';

// FIXME: NOT should move out maybe ?
export const binaryOperator = ({ binaryOperators, NOT }: SyntaxConfig) =>
  P.alt(...[...binaryOperators, NOT].map(operator => P.string(operator)));
