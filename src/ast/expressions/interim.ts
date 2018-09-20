import { List } from 'immutable';

import { Expression } from '../../common/model';
import { IExpression } from '../../dto';

export abstract class InterimExpression extends Expression {

  protected readonly name: string;

  public isValid(): boolean {
    throw Error(`${this.errorPrefix} Its validity is not known.`);
  }

  public checkTypes(): Expression {
    throw Error(`${this.errorPrefix} Its type is not known.`);
  }

  public checkIntegrity(): Expression {
    throw Error(`${this.errorPrefix} Its integrity is not known.`);
  }

  public toList(): List<Expression> {
    throw Error(`${this.errorPrefix} Can not be converted to a List.`);
  }

  public toJS(): IExpression {
    throw Error(`${this.errorPrefix} Can not be converted to a POJO.`);
  }

  private get errorPrefix() {
    return `${this.name} is a temporary construction.`;
  }

}
