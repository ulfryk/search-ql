import { MultiaryOperator } from './multiary-operator';

export class NotOperator extends MultiaryOperator {

  public static readonly one = new NotOperator();

  public toString() {
    return 'not';
  }

}
