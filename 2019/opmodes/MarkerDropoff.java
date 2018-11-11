package opmodes;

import relicopmodes.GlyphDeposits;
import team25core.DeadReckonPath;

/**
 * Created by Lizzie on 10/26/2018.
 */
public class MarkerDropoff {
    public enum StartingPosition {
        CLOSE,
        FAR,
        DEFAULT;
    }

    public enum DepotPath {
        LEFT,
        RIGHT,
        DEFAULT;
    }

    DeadReckonPath[][] paths = new DeadReckonPath[3][3];

    public MarkerDropoff() {
        paths[MarkerDropoff.StartingPosition.CLOSE.ordinal()][DepotPath.LEFT.ordinal()] = new DeadReckonPath();
        paths[MarkerDropoff.StartingPosition.CLOSE.ordinal()][DepotPath.LEFT.ordinal()].addSegment(DeadReckonPath.SegmentType.STRAIGHT, 17, 0.5);
        paths[MarkerDropoff.StartingPosition.CLOSE.ordinal()][DepotPath.LEFT.ordinal()].addSegment(DeadReckonPath.SegmentType.TURN, 90, -0.3);
        paths[MarkerDropoff.StartingPosition.CLOSE.ordinal()][DepotPath.LEFT.ordinal()].addSegment(DeadReckonPath.SegmentType.STRAIGHT, 40, 0.5);
        paths[MarkerDropoff.StartingPosition.CLOSE.ordinal()][DepotPath.LEFT.ordinal()].addSegment(DeadReckonPath.SegmentType.TURN, 90, 0.3);
        paths[MarkerDropoff.StartingPosition.CLOSE.ordinal()][DepotPath.LEFT.ordinal()].addSegment(DeadReckonPath.SegmentType.STRAIGHT, 45, 0.5);

        paths[MarkerDropoff.StartingPosition.CLOSE.ordinal()][DepotPath.RIGHT.ordinal()] = new DeadReckonPath();
        paths[MarkerDropoff.StartingPosition.CLOSE.ordinal()][DepotPath.RIGHT.ordinal()].addSegment(DeadReckonPath.SegmentType.STRAIGHT, 10, 0.5);
        paths[MarkerDropoff.StartingPosition.CLOSE.ordinal()][DepotPath.RIGHT.ordinal()].addSegment(DeadReckonPath.SegmentType.TURN, 65, 0.3);
        paths[MarkerDropoff.StartingPosition.CLOSE.ordinal()][DepotPath.RIGHT.ordinal()].addSegment(DeadReckonPath.SegmentType.STRAIGHT, 31, 0.5);
        paths[MarkerDropoff.StartingPosition.CLOSE.ordinal()][DepotPath.RIGHT.ordinal()].addSegment(DeadReckonPath.SegmentType.TURN, 221, -0.5);
        paths[MarkerDropoff.StartingPosition.CLOSE.ordinal()][DepotPath.RIGHT.ordinal()].addSegment(DeadReckonPath.SegmentType.STRAIGHT, 50, 0.5);


        paths[MarkerDropoff.StartingPosition.FAR.ordinal()][DepotPath.LEFT.ordinal()] = new DeadReckonPath();
        paths[MarkerDropoff.StartingPosition.FAR.ordinal()][DepotPath.LEFT.ordinal()].addSegment(DeadReckonPath.SegmentType.STRAIGHT, 8, 0.5);
        paths[MarkerDropoff.StartingPosition.FAR.ordinal()][DepotPath.LEFT.ordinal()].addSegment(DeadReckonPath.SegmentType.TURN, 93, -0.5);
        paths[MarkerDropoff.StartingPosition.FAR.ordinal()][DepotPath.LEFT.ordinal()].addSegment(DeadReckonPath.SegmentType.STRAIGHT, 42, 0.5);
        paths[MarkerDropoff.StartingPosition.FAR.ordinal()][DepotPath.LEFT.ordinal()].addSegment(DeadReckonPath.SegmentType.TURN, 47, -0.5);
        paths[MarkerDropoff.StartingPosition.FAR.ordinal()][DepotPath.LEFT.ordinal()].addSegment(DeadReckonPath.SegmentType.STRAIGHT, 50, -0.5);

        paths[MarkerDropoff.StartingPosition.FAR.ordinal()][DepotPath.RIGHT.ordinal()] = new DeadReckonPath();
        paths[MarkerDropoff.StartingPosition.FAR.ordinal()][DepotPath.RIGHT.ordinal()].addSegment(DeadReckonPath.SegmentType.STRAIGHT, 8, 0.5);
        paths[MarkerDropoff.StartingPosition.FAR.ordinal()][DepotPath.RIGHT.ordinal()].addSegment(DeadReckonPath.SegmentType.TURN, 93, -0.5);
        paths[MarkerDropoff.StartingPosition.FAR.ordinal()][DepotPath.RIGHT.ordinal()].addSegment(DeadReckonPath.SegmentType.STRAIGHT, 42, 0.5);
        paths[MarkerDropoff.StartingPosition.FAR.ordinal()][DepotPath.RIGHT.ordinal()].addSegment(DeadReckonPath.SegmentType.TURN, 47, -0.5);
        paths[MarkerDropoff.StartingPosition.FAR.ordinal()][DepotPath.RIGHT.ordinal()].addSegment(DeadReckonPath.SegmentType.STRAIGHT, 50, 0.5);
    }

    public DeadReckonPath getPath(MarkerDropoff.StartingPosition landing, MarkerDropoff.DepotPath path)
    {
        return paths[landing.ordinal()][path.ordinal()];
    }


}
