import { Map } from 'immutable';
import { Maybe, Some } from 'monet';

import { Expression, ExpressionType, IntegrityFailure, ValueType } from '../../../common/model';
import { ISelectorExpression } from '../../../dto';
import { InvalidExpression } from '../invalid';
import { PhraseExpression } from './phrase';
import { TermExpression } from './term';

export class SelectorExpression extends TermExpression {

  public static of(value: string) {
    return (matchingType: ValueType) =>
      new SelectorExpression(matchingType, value);
  }

  public readonly type: ExpressionType.Selector = ExpressionType.Selector;

  constructor(
    public readonly matchingType: ValueType,
    value: string,
  ) {
    super(value);
  }

  public toJS(): ISelectorExpression {
    return {
      matchingType: this.matchingType,
      returnType: this.returnType,
      type: this.type,
      value: this.value,
    };
  }

  public checkIntegrity(model: Map<string, ValueType>): Expression {
    return this.getIntegrityErrors(model)
      .map(errors => errors.map(IntegrityFailure.fromError(this)))
      .foldLeft(this as Expression)(InvalidExpression.fromErrors);
  }

  protected toPhrase(): TermExpression {
    return PhraseExpression.fromTerm(this);
  }

  private getIntegrityErrors(model: Map<string, ValueType>): Maybe<string[]> {
    return Some([
      ...this.getNotInModelError(model),
      ...this.getNotMatchingModelTypeError(model),
    ]).filter(errors => errors.length > 0);
  }

  private getNotInModelError(model: Map<string, ValueType>) {
    return model.has(this.value) ? [] : [
      `SelectorExpression is invalid - "${this.value}" is ` +
        'not available on model definition',
    ];
  }

  private getNotMatchingModelTypeError(model: Map<string, ValueType>) {
    return model.get(this.value) === this.matchingType ? [] :
      model.has(this.value) ? [
        'SelectorExpression is invalid - "matchingType" should equal ' +
          `${model.get(this.value)} but is ${this.matchingType}`,
      ] : [];
  }

}
