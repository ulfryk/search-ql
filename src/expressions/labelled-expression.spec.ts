/* tslint:disable:no-unused-expression no-magic-numbers no-import-side-effect  */
import '@samwise-tech/immutable';
import { expect } from 'chai';
import * as _ from 'lodash';

import { BasicExpression } from './basic-expression';
import { Expression } from './expression';
import { LabelledExpression } from './labelled-expression';

describe('SearchQL expressions', () => {

  describe('LabelledExpression', () => {

    describe('equals() method', () => {

      const lhs = [
        new LabelledExpression('label', BasicExpression.fromMatch('text')),
        new LabelledExpression('theLabel', BasicExpression.fromMatch('')),
        new LabelledExpression('', BasicExpression.fromMatch('text')),
      ];

      const rhs = [
        new LabelledExpression('label', new BasicExpression('text')),
        new LabelledExpression('theLabel', new BasicExpression('')),
        new LabelledExpression('', new BasicExpression('text')),
      ];

      const rhsInvalid = [
        new LabelledExpression('label', new BasicExpression('')),
        new LabelledExpression('label', new BasicExpression('')),
        new LabelledExpression('text', new BasicExpression('label')),
      ];

      it('should return true for comparison with a reference', () => {
        lhs.concat(rhs).concat(rhsInvalid).forEach(expression => {
          expect(expression.equals(expression)).to.be.true;
        });
      });

      it('should return true for different instances of same shape', () => {
        _.zip<Expression>(lhs, rhs).forEach(([left, right]) => {
          expect(left.equals(right)).to.be.true;
        });
      });

      it('should return false for instances of different shape', () => {
        _.zip<Expression>(lhs, rhsInvalid).forEach(([left, right]) => {
          expect(left.equals(right)).to.be.false;
        });
      });

    });

  });

});
