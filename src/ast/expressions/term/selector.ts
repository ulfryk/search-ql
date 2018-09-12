import { Map } from 'immutable';
import { Maybe } from 'monet';

import { ValueType } from '../../../common/model';
import { TermExpression } from './term';

export class SelectorExpression extends TermExpression<string> {

  public static fromTerm(model: Map<string, ValueType>) {
    return (term: TermExpression) =>
      Maybe.fromNull(model.get(term.value))
        .fold(term)(SelectorExpression.of(term.value));
  }

  public static of(value: string) {
    return (matchingType: ValueType) =>
      new SelectorExpression(matchingType, value);
  }

  constructor(
    public readonly matchingType: ValueType,
    value: string,
  ) {
    super(value, value.trim());
  }

}
