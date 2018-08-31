import { ValueType } from '../../../common/model';
import { TermExpression } from './term';

export class NumberExpression extends TermExpression<number> {

  public static fromMatch(match: string) {
    return new NumberExpression(match);
  }

  // TODO: probably use config here
  public static prepareValue(value: string) {
    return Number(value.trim());
  }

  public readonly returnType = ValueType.Number;

  constructor(value: string) {
    super(value, NumberExpression.prepareValue(value));
  }

}
