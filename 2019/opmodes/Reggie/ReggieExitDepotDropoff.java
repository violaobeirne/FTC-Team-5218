package opmodes;

import team25core.DeadReckonPath;

/**
 * Created by Lizzie on 11/27/2018.
 */
public class ReggieExitDepotDropoff {

    DeadReckonPath[] paths = new DeadReckonPath[3];

    public ReggieExitDepotDropoff() {
        // true paths tested on Monday
        paths[ReggieDropoffUtil.MineralPosition.LEFT.ordinal()] = new DeadReckonPath();
        paths[ReggieDropoffUtil.MineralPosition.LEFT.ordinal()].addSegment(DeadReckonPath.SegmentType.STRAIGHT, 8, -0.5);
        // straight, 100, -1.0

        paths[ReggieDropoffUtil.MineralPosition.CENTER.ordinal()] = new DeadReckonPath();
        paths[ReggieDropoffUtil.MineralPosition.CENTER.ordinal()].addSegment(DeadReckonPath.SegmentType.STRAIGHT, 8, -0.5);
        // straight, 100, -1.0

        paths[ReggieDropoffUtil.MineralPosition.RIGHT.ordinal()] = new DeadReckonPath();
        paths[ReggieDropoffUtil.MineralPosition.RIGHT.ordinal()].addSegment(DeadReckonPath.SegmentType.STRAIGHT, 8, -0.5);
    }

    public DeadReckonPath getPath(ReggieDropoffUtil.MineralPosition goldPosition)
    {
        return paths[goldPosition.ordinal()];
    }
}
