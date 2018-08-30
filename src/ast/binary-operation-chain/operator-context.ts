import { compareNum, IOrd, OperatorAssociativity, Ordering } from '../../common/model';
import { BinaryOperator } from '../operators';

export class OperatorContext implements IOrd<OperatorContext> {

  constructor(
    public readonly index: number,
    public readonly operator: BinaryOperator,
  ) {}

  public get rightIndex() {
    return this.index + 1;
  }

  public equals(other: OperatorContext) {
    return this === other || (
      this.index === other.index &&
      this.operator.equals(other.operator)
    );
  }

  // tslint:disable-next-line:cyclomatic-complexity
  public compare(other: OperatorContext): Ordering {
    if (other.operator.precedence !== this.operator.precedence) {
      return compareNum(this.operator.precedence, other.operator.precedence);
    }
    if (this.operator.associativity !== other.operator.associativity) {
      throw Error('No 2 operators with equal precedence but different associativity exist.');
    }
    // should lead to creating InvalidExpression
    if (this.operator.associativity === OperatorAssociativity.None) {
      throw Error('Cannot chain non-associative operators');
    }
    return this.operator.associativity === OperatorAssociativity.Left ?
      compareNum(other.index, this.index) :
      compareNum(this.index, other.index);
  }

  public toString() {
    return `${this.operator}[${this.index}]`;
  }

}
