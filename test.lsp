(defun minimax (player board ply eval-fn)
  (if (= ply 0)
      (funcall eval-fn player board) ; プレイアウトの深さが0に達したら評価関数を呼び出して評価値を返す
      (let ((moves (legal-moves player board))) ; プレイヤーが可能な手を取得
        (if (null moves) ; プレイヤーが手を打つことができない場合
            (if (any-legal-move? (opponent player) board) ; 相手プレイヤーが打てる手があるかをチェック
                (- (minimax (opponent player) board (- ply 1) eval-fn)) ; 相手プレイヤーの手番に切り替えて再帰的に探索
                (final-value player board)) ; 相手プレイヤーも手を打てない場合、ゲームの最終評価値を計算
            (let ((best-move nil) (best-val nil))
              (dolist (move moves) ; プレイヤーが可能な手をループ
                (let* ((board2 (make-move move player (copy-board board))) ; 手を打った後の新しいボードを生成
                       (val (- (minimax (opponent player) board2 (- ply 1) eval-fn)))) ; 相手プレイヤーの手番に切り替えて再帰的に探索
                  (when (or (null best-val) ; 最良の手を見つけた場合、best-moveとbest-valを更新
                            (> val best-val))
                    (setf best-val val)
                    (setf best-move move))))
              (values best-val best-move)))))) ; 最適な手の評価値と手を返す