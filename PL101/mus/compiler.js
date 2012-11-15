/* A compiler for MUS -> NOTE for Nathan's PL101. */

var endTime = function(start, expr) {
  if (expr.tag == 'note' || expr.tag == 'rest')
    return start + expr.dur;
 
  if (expr.tag == 'repeat') {
    length = endTime(start, expr.section) - start;
    return start + length * expr.count;
  }

  var e_left = endTime(start, expr.left);
  var e_right; 
  if (expr.tag == 'seq')
    e_right = endTime(e_left, expr.right); 
  else if (expr.tag == 'par')
    e_right = endTime(start, expr.right);

  return e_left > e_right ? e_left : e_right;
};

var convertPitch = function(pitch) {
  var letterPitch = { 'c':0, 'd':2, 'e':4, 'f':5, 'g':7, 'a':9, 'b':11 };
  return 12 + (12 * pitch[1]) + letterPitch[pitch[0]];
};

var compile = function(expr) {
  var innerCompile;
  innerCompile = function(start, expr) {
    if (expr.tag == 'note') {
      return [ { tag: expr.tag,
                 pitch: convertPitch(expr.pitch),
                 start: start,
                 dur: expr.dur } ];
    }

    if (expr.tag == 'rest') {
      return [ { tag: expr.tag,
                 pitch: 0,
                 start: start,
                 dur: expr.dur } ];
    }

    if (expr.tag == 'repeat') {
      var time = start;
      var notes = [];
      for (var i = 0; i < expr.count; i++) {
        notes = notes.concat(innerCompile(time, expr.section));
        time = endTime(time, expr.section);
      }
      return notes;
    }

    var c_left = innerCompile(start, expr.left);
    var c_right;
    if (expr.tag == 'seq')
      c_right = innerCompile(endTime(start, expr.left), expr.right);
    else if (expr.tag == 'par')
      c_right = innerCompile(start, expr.right);
    return c_left.concat(c_right);
  };
  return innerCompile(0, expr);
};

var melody_mus = {
  tag: 'seq',
  left:
    { tag: 'repeat',
      count: 3,
      section: { tag: 'seq',
                 left:  { tag: 'note', pitch: 'a6', dur: 250 },
                 right: { tag: 'rest', dur: 250 } } },
  right:
    { tag: 'par',
      left:  { tag: 'note', pitch: 'c4', dur: 500 },
      right: { tag: 'note', pitch: 'd4', dur: 500 } }
};

console.log(melody_mus);
console.log(compile(melody_mus));

