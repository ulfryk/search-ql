import { Map } from 'immutable';

import { compareNum, DateExpression, IOrd, NumberExpression, Ordering, SelectorExpression, ValueType } from '../../../index';

import { NodeEvaluation } from '../../model';

const normalizeValue = <E>(v: string, type: ValueType) => {
  if (type === ValueType.Date) {
    return DateExpression.prepareValue(v) as any as E;
  }

  if (type === ValueType.Number) {
    return NumberExpression.prepareValue(v) as any as E;
  }

  if (type === ValueType.Boolean) {
    return v.trim() as any as E;
  }

  return v as any as E;
};

export class RelationValue<T> implements IOrd<RelationValue<T>> {

  public static fromEvaluation<E>(
    side: NodeEvaluation<E>,
    values: Map<string, string>,
  ) {
    const type = side.node instanceof SelectorExpression ?
      side.node.matchingType :
      side.type;

    return new RelationValue<E>(
      side.node instanceof SelectorExpression ?
        // probably value should be parsed as date/number or trimmed
        normalizeValue<E>(values.get(side.value as any as string), type) :
        side.value,
        type);
  }

  constructor(
    public readonly value: T,
    public readonly type: ValueType,
  ) {}

  // tslint:disable-next-line:cyclomatic-complexity
  public compare(other: RelationValue<T>): Ordering {
    if (this.type !== other.type) {
      throw TypeError(
        'Both sides of relation operation have to have same type. ' +
        `Got: ${this.type} and ${other.type}`);
    }

    if ([ValueType.Date, ValueType.Number].includes(this.type)) {
      return compareNum(this.value as any as number, other.value as any as number);
    }

    if (this.equals(other)) {
      return Ordering.Eq;
    }

    if (this.value > other.value) {
      return Ordering.Gt;
    }

    return Ordering.Lt;

  }

  public equals(other: RelationValue<T>) {
    return this === other || this.value === other.value;
  }

}
