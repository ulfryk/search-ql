import { Map } from 'immutable';
import { Maybe, Some } from 'monet';

import { Expression, ExpressionType, ValueType } from '../../../common/model';
import { ISelectorExpression } from '../../../dto';
import { InvalidExpression } from '../invalid';
import { PhraseExpression } from './phrase';
import { TermExpression } from './term';

export class SelectorExpression extends TermExpression<string> {

  public static of(value: string) {
    return (matchingType: ValueType) =>
      new SelectorExpression(matchingType, value);
  }

  public readonly type = ExpressionType.Selector;

  constructor(
    public readonly matchingType: ValueType,
    value: string,
    preparedValue = value.trim(),
  ) {
    super(value, preparedValue);
  }

  public toJS(): ISelectorExpression {
    return {
      matchingType: this.matchingType,
      preparedValue: this.preparedValue,
      returnType: this.returnType,
      type: this.type,
      value: this.value,
    };
  }

  public checkIntegrity(model: Map<string, ValueType>): Expression {
    return this.getIntegrityErrors(model)
      .foldLeft(this as Expression)(InvalidExpression.fromErrors);
  }

  protected toPhrase(): TermExpression {
    return PhraseExpression.fromTerm(this);
  }

  private getIntegrityErrors(model: Map<string, ValueType>): Maybe<string[]> {
    return Some([
      ...this.getNotInModelError(model),
      ...this.getNotMatchingModelTypeError(model),
      ...this.getNotMatchingValuesError(),
    ]).filter(errors => errors.length > 0);
  }

  private getNotInModelError(model: Map<string, ValueType>) {
    return model.has(this.preparedValue) ? [] : [
      `SelectorExpression is invalid ${this.preparedValue} is ` +
        'not available on model definition',
    ];
  }

  private getNotMatchingModelTypeError(model: Map<string, ValueType>) {
    return model.get(this.preparedValue) === this.matchingType ? [] :
      model.has(this.preparedValue) ? [
        'SelectorExpression is invalid - "matchingType" should equal ' +
          `${model.get(this.preparedValue)} but is ${this.matchingType}`,
      ] : [];
  }

  private getNotMatchingValuesError() {
    return this.value.trim() === this.preparedValue ? [] : [
      'Values of SelectorExpression doesn\'t match: ' +
        `{ value: ${this.value.trim()}, preparedValue: ${this.preparedValue}}`,
    ];
  }

}
