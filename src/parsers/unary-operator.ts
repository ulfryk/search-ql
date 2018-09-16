import * as P from 'parsimmon';

import { ParserConfig } from '../config';

// FIXME: NOT should move out maybe ?
export const unaryOperator = ({ unaryOperators }: ParserConfig) =>
  P.alt(...unaryOperators.map(operator => P.string(operator)));
