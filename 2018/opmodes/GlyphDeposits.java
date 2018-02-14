package opmodes;

import java.lang.annotation.Target;

import team25core.DeadReckonPath;

/**
 * Created by Lizzie on 12/23/2017.
 */

public class GlyphDeposits {
    public enum StartingPosition {
        R1,
        R2,
        B1,
        B2,
        DEFAULT;
    }

    public enum TargetColumn {
        LEFT,
        RIGHT,
        MIDDLE,
        DEFAULT;
    }

    DeadReckonPath[][] paths = new DeadReckonPath[5][4];

    public GlyphDeposits()
    {
        paths[StartingPosition.R1.ordinal()][TargetColumn.LEFT.ordinal()] = new DeadReckonPath();
        paths[StartingPosition.R1.ordinal()][TargetColumn.LEFT.ordinal()].addSegment(DeadReckonPath.SegmentType.STRAIGHT, 48, 0.5);
        paths[StartingPosition.R1.ordinal()][TargetColumn.LEFT.ordinal()].addSegment(DeadReckonPath.SegmentType.TURN, 75, 0.3);
        paths[StartingPosition.R1.ordinal()][TargetColumn.LEFT.ordinal()].addSegment(DeadReckonPath.SegmentType.STRAIGHT, 8, 0.5);

        paths[StartingPosition.R1.ordinal()][TargetColumn.MIDDLE.ordinal()] = new DeadReckonPath();
        paths[StartingPosition.R1.ordinal()][TargetColumn.MIDDLE.ordinal()].addSegment(DeadReckonPath.SegmentType.STRAIGHT, 38, 0.5);
        paths[StartingPosition.R1.ordinal()][TargetColumn.MIDDLE.ordinal()].addSegment(DeadReckonPath.SegmentType.TURN, 76, 0.3);
        paths[StartingPosition.R1.ordinal()][TargetColumn.MIDDLE.ordinal()].addSegment(DeadReckonPath.SegmentType.STRAIGHT, 8, 0.5);

        paths[StartingPosition.R1.ordinal()][TargetColumn.RIGHT.ordinal()] = new DeadReckonPath();
        paths[StartingPosition.R1.ordinal()][TargetColumn.RIGHT.ordinal()].addSegment(DeadReckonPath.SegmentType.STRAIGHT, 33, 0.5);
        paths[StartingPosition.R1.ordinal()][TargetColumn.RIGHT.ordinal()].addSegment(DeadReckonPath.SegmentType.TURN, 70, 0.3);
        paths[StartingPosition.R1.ordinal()][TargetColumn.RIGHT.ordinal()].addSegment(DeadReckonPath.SegmentType.STRAIGHT, 8, 0.5);

        paths[StartingPosition.R2.ordinal()][TargetColumn.RIGHT.ordinal()] = new DeadReckonPath();
        paths[StartingPosition.R2.ordinal()][TargetColumn.RIGHT.ordinal()].addSegment(DeadReckonPath.SegmentType.STRAIGHT, 15, 0.5);
        paths[StartingPosition.R2.ordinal()][TargetColumn.RIGHT.ordinal()].addSegment(DeadReckonPath.SegmentType.TURN, 15, -0.3);
        paths[StartingPosition.R2.ordinal()][TargetColumn.RIGHT.ordinal()].addSegment(DeadReckonPath.SegmentType.STRAIGHT, 20, 0.5);

        paths[StartingPosition.R2.ordinal()][TargetColumn.MIDDLE.ordinal()] = new DeadReckonPath();
        paths[StartingPosition.R2.ordinal()][TargetColumn.MIDDLE.ordinal()].addSegment(DeadReckonPath.SegmentType.STRAIGHT, 20, 0.5);
        paths[StartingPosition.R2.ordinal()][TargetColumn.MIDDLE.ordinal()].addSegment(DeadReckonPath.SegmentType.TURN, 85, -0.3);
        paths[StartingPosition.R2.ordinal()][TargetColumn.MIDDLE.ordinal()].addSegment(DeadReckonPath.SegmentType.STRAIGHT, 10, 0.5);
        paths[StartingPosition.R2.ordinal()][TargetColumn.MIDDLE.ordinal()].addSegment(DeadReckonPath.SegmentType.TURN, 53, 0.3);
        paths[StartingPosition.R2.ordinal()][TargetColumn.MIDDLE.ordinal()].addSegment(DeadReckonPath.SegmentType.STRAIGHT, 8, 0.5);

        paths[StartingPosition.R2.ordinal()][TargetColumn.LEFT.ordinal()] = new DeadReckonPath();
        paths[StartingPosition.R2.ordinal()][TargetColumn.LEFT.ordinal()].addSegment(DeadReckonPath.SegmentType.STRAIGHT, 20, 0.5);
        paths[StartingPosition.R2.ordinal()][TargetColumn.LEFT.ordinal()].addSegment(DeadReckonPath.SegmentType.TURN, 85, -0.3);
        paths[StartingPosition.R2.ordinal()][TargetColumn.LEFT.ordinal()].addSegment(DeadReckonPath.SegmentType.STRAIGHT, 23, 0.5);
        paths[StartingPosition.R2.ordinal()][TargetColumn.LEFT.ordinal()].addSegment(DeadReckonPath.SegmentType.TURN, 60, 0.3);
        paths[StartingPosition.R2.ordinal()][TargetColumn.LEFT.ordinal()].addSegment(DeadReckonPath.SegmentType.STRAIGHT, 8, 0.5);

        paths[StartingPosition.B1.ordinal()][TargetColumn.LEFT.ordinal()] = new DeadReckonPath();
        paths[StartingPosition.B1.ordinal()][TargetColumn.LEFT.ordinal()].addSegment(DeadReckonPath.SegmentType.STRAIGHT, 30, -0.5);
        paths[StartingPosition.B1.ordinal()][TargetColumn.LEFT.ordinal()].addSegment(DeadReckonPath.SegmentType.TURN, 75, 0.3);
        paths[StartingPosition.B1.ordinal()][TargetColumn.LEFT.ordinal()].addSegment(DeadReckonPath.SegmentType.STRAIGHT, 8, 0.5);

        paths[StartingPosition.B1.ordinal()][TargetColumn.MIDDLE.ordinal()] = new DeadReckonPath();
        paths[StartingPosition.B1.ordinal()][TargetColumn.MIDDLE.ordinal()].addSegment(DeadReckonPath.SegmentType.STRAIGHT, 38, -0.5);
        paths[StartingPosition.B1.ordinal()][TargetColumn.MIDDLE.ordinal()].addSegment(DeadReckonPath.SegmentType.TURN, 76, 0.3);
        paths[StartingPosition.B1.ordinal()][TargetColumn.MIDDLE.ordinal()].addSegment(DeadReckonPath.SegmentType.STRAIGHT, 8, 0.5);

        paths[StartingPosition.B1.ordinal()][TargetColumn.RIGHT.ordinal()] = new DeadReckonPath();
        paths[StartingPosition.B1.ordinal()][TargetColumn.RIGHT.ordinal()].addSegment(DeadReckonPath.SegmentType.STRAIGHT, 48, -0.5);
        paths[StartingPosition.B1.ordinal()][TargetColumn.RIGHT.ordinal()].addSegment(DeadReckonPath.SegmentType.TURN, 75, 0.3);
        paths[StartingPosition.B1.ordinal()][TargetColumn.RIGHT.ordinal()].addSegment(DeadReckonPath.SegmentType.STRAIGHT, 8, 0.5);

        paths[StartingPosition.B2.ordinal()][TargetColumn.LEFT.ordinal()] = new DeadReckonPath();
        paths[StartingPosition.B2.ordinal()][TargetColumn.LEFT.ordinal()].addSegment(DeadReckonPath.SegmentType.STRAIGHT, 15, -0.5);
        paths[StartingPosition.B2.ordinal()][TargetColumn.LEFT.ordinal()].addSegment(DeadReckonPath.SegmentType.TURN, 140, -0.3);
        paths[StartingPosition.B2.ordinal()][TargetColumn.LEFT.ordinal()].addSegment(DeadReckonPath.SegmentType.STRAIGHT, 15, 0.5);

        paths[StartingPosition.B2.ordinal()][TargetColumn.MIDDLE.ordinal()] = new DeadReckonPath();
        paths[StartingPosition.B2.ordinal()][TargetColumn.MIDDLE.ordinal()].addSegment(DeadReckonPath.SegmentType.STRAIGHT, 20, -0.5);
        paths[StartingPosition.B2.ordinal()][TargetColumn.MIDDLE.ordinal()].addSegment(DeadReckonPath.SegmentType.TURN, 85, -0.3);
        paths[StartingPosition.B2.ordinal()][TargetColumn.MIDDLE.ordinal()].addSegment(DeadReckonPath.SegmentType.STRAIGHT, 10, 0.5);
        paths[StartingPosition.B2.ordinal()][TargetColumn.MIDDLE.ordinal()].addSegment(DeadReckonPath.SegmentType.TURN, 53, -0.3);
        paths[StartingPosition.B2.ordinal()][TargetColumn.MIDDLE.ordinal()].addSegment(DeadReckonPath.SegmentType.STRAIGHT, 8, 0.5);

        paths[StartingPosition.B2.ordinal()][TargetColumn.RIGHT.ordinal()] = new DeadReckonPath();
        paths[StartingPosition.B2.ordinal()][TargetColumn.RIGHT.ordinal()].addSegment(DeadReckonPath.SegmentType.STRAIGHT, 20, -0.5);
        paths[StartingPosition.B2.ordinal()][TargetColumn.RIGHT.ordinal()].addSegment(DeadReckonPath.SegmentType.TURN, 85, -0.3);
        paths[StartingPosition.B2.ordinal()][TargetColumn.RIGHT.ordinal()].addSegment(DeadReckonPath.SegmentType.STRAIGHT, 23, 0.5);
        paths[StartingPosition.B2.ordinal()][TargetColumn.RIGHT.ordinal()].addSegment(DeadReckonPath.SegmentType.TURN, 60, -0.3);
        paths[StartingPosition.B2.ordinal()][TargetColumn.RIGHT.ordinal()].addSegment(DeadReckonPath.SegmentType.STRAIGHT, 8, 0.5);
    }

    public DeadReckonPath getPath(StartingPosition stone, TargetColumn column)
    {
        return paths[stone.ordinal()][column.ordinal()];
    }

}
