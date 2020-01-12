package opmodes;

import com.qualcomm.hardware.rev.RevColorSensorV3;
import com.qualcomm.robotcore.hardware.CRServo;
import com.qualcomm.robotcore.hardware.ColorSensor;
import com.qualcomm.robotcore.hardware.DcMotor;
import com.qualcomm.robotcore.hardware.Servo;
import com.qualcomm.robotcore.util.ElapsedTime;

import org.firstinspires.ftc.robotcontroller.external.samples.SensorREVColorDistance;

import opmodes.calibration.MiyazakiCalibration;
import team25core.ColorSensorTask;
import team25core.DeadReckonPath;
import team25core.DeadReckonTask;
import team25core.MotorStallTask;
import team25core.RobotEvent;
import team25core.SingleShotColorSensorTask;
import team25core.Robot;
import team25core.RobotTask;
import test.SkystoneDetectionTask;

/**
 * Created by Lizzie on 12/19/2019.
 */
public class AutomatedLiftTask extends RobotTask {

    public enum EventKind {
        DONE,
    }

    public class AutomatedLiftEvent extends RobotEvent {

        public AutomatedLiftTask.EventKind kind;

        public AutomatedLiftEvent(RobotTask task, AutomatedLiftTask.EventKind k)
        {
            super(task);
            kind = k;
        }

        @Override
        public String toString()
        {
            return (super.toString() + "AutomatedLift Event " + kind);
        }
    }

    public enum LiftStates {
        // needs to be changed to lift states -- implemented state machine in time slice
        // needs enum, switch statement w/ case labls = num values, and set of instructions
        LIMBO,
        LIFTING,
        START_HLIFT_EXTEND,
        EXTENDING,
        DROPPING,
        WAIT_FOR_DROP,
        START_HLIFT_RETRACT,
        RETRACTING,
        LOWERING,
        STOPPING,
    }

    protected DcMotor vLift;
    protected CRServo hLift;
    protected Servo claw;
    protected RevColorSensorV3 colorSensor;
    protected int level;
    protected int levelEncoderValue;
    protected LiftStates state;

    protected ColorSensorTask colorSensorTask;
    protected ElapsedTime timer;

    public AutomatedLiftTask(Robot robot, DcMotor vLift, CRServo hLift, Servo claw, RevColorSensorV3 colorSensor, int levelEncoderValue)
    {
        super(robot);

        this.vLift = vLift;
        vLift.setMode(DcMotor.RunMode.RUN_USING_ENCODERS);
        this.hLift = hLift;
        this.claw = claw;
        this.colorSensor = colorSensor;
        this.levelEncoderValue = levelEncoderValue;
        this.level = 0;
        state = LiftStates.LIMBO;
    }

    @Override
    public void start()
    {
    }

    @Override
    public void stop()
    {
        robot.removeTask(this);
    }

    public void doStoneStack()
    {
        state = LiftStates.LIFTING;
    }

    @Override
    public boolean timeslice() {

        switch (state) {
            case LIMBO:
                break;
            case LIFTING: // run vertical lift motor up, check some encoder value multiplier, state = extend, turn off motor
                vLift.setPower(MiyazakiCalibration.VLIFT_UP);
                if  (vLift.getCurrentPosition() >= (levelEncoderValue * level)) {
                    vLift.setPower(0.0); // doesn't this need to be 0? would setTarget hold the motor?
                    state = LiftStates.START_HLIFT_EXTEND;
                }
               break;
            case START_HLIFT_EXTEND: // run horizontal CR servo out, check for color, state = extend, turn off servo
                hLift.setPower(MiyazakiCalibration.HLIFT_OUT);
                colorSensorTask = new ColorSensorTask(robot, colorSensor) {
                    public void handleEvent(RobotEvent e) {
                        ColorSensorTask.ColorSensorEvent event = (ColorSensorTask.ColorSensorEvent) e;
                        switch(event.kind) {
                            // red is at the end
                            case RED_DETECTED:
                                colorSensorTask.suspend();
                                hLift.setPower(0.0);
                                state = LiftStates.DROPPING;
                                break;
                            case BLUE_DETECTED:
                                hLift.setPower(0.0);
                                robot.removeTask(colorSensorTask);
                                state = LiftStates.LOWERING;
                        }
                    }
                };
                robot.addTask(colorSensorTask);
                state = LiftStates.EXTENDING;
                break;
            case EXTENDING:
                /*
                 * We are waiting for the color sensor to detect red.
                 */
                break;
            case DROPPING:
                // run to claw position, state = retract
                timer = new ElapsedTime(ElapsedTime.Resolution.MILLISECONDS);
                claw.setPosition(MiyazakiCalibration.NEW_CLAW_OPEN);
                state = LiftStates.WAIT_FOR_DROP;
                break;
            case WAIT_FOR_DROP:
                if (timer.time() >= 1000) {
                    state = LiftStates.START_HLIFT_RETRACT;
                }
            case START_HLIFT_RETRACT:
                hLift.setPower(MiyazakiCalibration.HLIFT_IN);
                colorSensorTask.resume();
                state = LiftStates.RETRACTING;
                break;
            case RETRACTING:
                /*
                 * We are waiting for the color sensor to detect blue.
                 */
                break;
            case LOWERING:
                // turn on vertical lift motor, check for encoder value is current level * encoder level value, state = retract, turn off motor, level = 0
                vLift.setPower(MiyazakiCalibration.VLIFT_DOWN);

                // robot.addTask(MotorStallTask());
                if (vLift.getCurrentPosition() <= 0) {
                    vLift.setPower(0.0);
                    vLift.setMode(DcMotor.RunMode.RESET_ENCODERS);
                    state = LiftStates.STOPPING;
                }
                level++;
                break;
            case STOPPING:
                /*
                 * Send event indicating done.
                 */
                robot.queueEvent(new AutomatedLiftEvent(this, EventKind.DONE));
                state = LiftStates.LIMBO;
                break;
        }

        return false;
    }
}
