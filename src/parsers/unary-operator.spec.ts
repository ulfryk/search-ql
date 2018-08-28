/* tslint:disable:no-unused-expression */
import { expect } from 'chai';
import * as P from 'parsimmon';

import { SyntaxConfig } from '../config';
import { unaryOperator } from './unary-operator';

const config = new SyntaxConfig();
const { NOT } = config;

describe('SearchQL parsers', () => {

  describe('unaryOperator', () => {

    const validInput = [...NOT];

    validInput.forEach(input => {
      describe(`for '${input}'`, () => {
        const parsed = unaryOperator(config).parse(input);

        it('should succeed', () => {
          expect(parsed.status).to.be.true;
        });

        it('should provide proper value', () => {
          expect(parsed.status ? parsed.value : null).to.equal(input);
        });

      });
    });

    ['XX', 'NEXT', 'YXZ'].forEach(input => {
      describe(`for '${input}'`, () => {

        it('should fail', () => {
          expect(unaryOperator(config).parse(input).status).to.be.false;
        });

      });
    });

    describe('for many valid occurrences', () => {
      const input = validInput.join(' ');
      const parser = P.sepBy1(unaryOperator(config), P.whitespace);
      const expectedOutput = validInput;
      const output = parser.parse(input);

      it('should succeed', () => {
        expect(output.status).to.be.true;
      });

      it('should provide proper value', () => {
        expect(output.status ? output.value : null).to.deep.equal(expectedOutput);
      });

    });

    describe('for mixed input (valid occurrences mixed with invalid strings)', () => {
      const input = validInput.concat(['XX', 'NEXT', 'YXZ']).join(' ');
      const parser = P.sepBy1(unaryOperator(config), P.whitespace);
      const output = parser.parse(input);

      it('should fail', () => {
        expect(output.status).to.be.false;
      });

    });

  });

});
