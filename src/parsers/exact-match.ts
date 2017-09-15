import * as P from 'parsimmon';

import { BasicExpression } from '../expressions';
import { EXACT_MATCHER } from '../syntax-config';

const startEnd = P.string(EXACT_MATCHER);
const content = P.regexp(new RegExp(`[^${EXACT_MATCHER}]+`));

const exactMatch = P.seqMap(startEnd, content, startEnd,
  (_start, regExMatches, _end) => regExMatches)
  .map(BasicExpression.fromMatch);

export { exactMatch };
