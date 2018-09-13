import { Maybe } from 'monet';

import { ParserConfig } from './../../../config/parser-config';
import { DateExpression } from './date';
import { isDate } from './is-date';
import { isNumber } from './is-number';
import { NumberExpression } from './number';
import { SelectorExpression } from './selector';
import { TermExpression } from './term';
import { TextExpression } from './text';

export const fromMatch = ({ model }: ParserConfig) => (match: string): TermExpression =>
  Maybe.fromNull(model.get(match))
    .cata<TermExpression>(() => {
      // TODO: Allow passing custom number matchers/parsers with config
      if (isNumber(match)) {
        return NumberExpression.of(match);
      }

      // TODO: Allow passing custom date matchers/parsers with config
      if (isDate(match)) {
        return DateExpression.of(match);
      }

      return TextExpression.of(match);
    }, SelectorExpression.of(match));
